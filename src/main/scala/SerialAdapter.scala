package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.chip._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange, LazyMultiIOModuleImp}
import freechips.rocketchip.rocket.PAddrBits
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.tilelink.{TLClientNode, TLClientParameters}
import freechips.rocketchip.util._
import scala.math.min

case object SerialInterfaceWidth extends Field[Int]

class SerialAdapter(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(TLClientParameters(
    name = "serial", sourceId = IdRange(0,1)))

  lazy val module = new SerialAdapterModule(this)
}

class SerialAdapterModule(outer: SerialAdapter) extends LazyModuleImp(outer) {
  val w = p(SerialInterfaceWidth)
  val io = IO(new Bundle {
    val serial = new SerialIO(w)
    val mem = outer.node.bundleOut
  })

  val pAddrBits = p(PAddrBits)
  val xLen = p(XLen)
  val wordBytes = xLen / 8
  val nChunksPerWord = xLen / w
  val byteAddrBits = log2Ceil(wordBytes)

  require(nChunksPerWord > 0, s"Serial interface width must be <= PAddrBits $pAddrBits")

  val cmd = Reg(UInt(w.W))
  val addr = Reg(UInt(xLen.W))
  val len = Reg(UInt(xLen.W))
  val body = Reg(Vec(nChunksPerWord, UInt(w.W)))
  val bodyValid = Reg(UInt(nChunksPerWord.W))
  val idx = Reg(UInt(log2Up(nChunksPerWord).W))

  val (cmd_read :: cmd_write :: Nil) = Enum(2)
  val (s_cmd :: s_addr :: s_len ::
       s_read_req  :: s_read_data :: s_read_body :: 
       s_write_body :: s_write_data :: s_write_ack :: Nil) = Enum(9)
  val state = RegInit(s_cmd)

  io.serial.in.ready := state.isOneOf(s_cmd, s_addr, s_len, s_write_body)
  io.serial.out.valid := state === s_read_body
  io.serial.out.bits := body(idx)

  val beatAddr = addr(pAddrBits - 1, byteAddrBits)
  val nextAddr = Cat(beatAddr + 1.U, 0.U(byteAddrBits.W))

  val wmask = FillInterleaved(w/8, bodyValid)
  val addr_size = nextAddr - addr
  val len_size = Cat(len + 1.U, 0.U(log2Ceil(w/8).W))
  val raw_size = Mux(len_size < addr_size, len_size, addr_size)
  val rsize = MuxLookup(raw_size, byteAddrBits.U,
    (0 until log2Ceil(wordBytes)).map(i => ((1 << i).U -> i.U)))

  val pow2size = PopCount(raw_size) === 1.U
  val byteAddr = Mux(pow2size, addr(byteAddrBits - 1, 0), 0.U)

  val mem = io.mem.head
  val edge = outer.node.edgesOut(0)

  val put_acquire = edge.Put(
    0.U, beatAddr << byteAddrBits.U, log2Ceil(wordBytes).U,
    body.asUInt, wmask)._2

  val get_acquire = edge.Get(
    0.U, Cat(beatAddr, byteAddr), rsize)._2

  mem.a.valid := state.isOneOf(s_write_data, s_read_req)
  mem.a.bits := Mux(state === s_write_data, put_acquire, get_acquire)
  mem.b.ready := false.B
  mem.c.valid := false.B
  mem.d.ready := state.isOneOf(s_write_ack, s_read_data)
  mem.e.valid := false.B

  def shiftBits(bits: UInt, idx: UInt): UInt =
    bits << Cat(idx, 0.U(log2Up(w).W))

  def addrToIdx(addr: UInt): UInt =
    addr(byteAddrBits - 1, log2Up(w/8))

  when (state === s_cmd && io.serial.in.valid) {
    cmd := io.serial.in.bits
    idx := 0.U
    addr := 0.U
    len := 0.U
    state := s_addr
  }

  when (state === s_addr && io.serial.in.valid) {
    val addrIdx = idx(log2Up(nChunksPerWord) - 1, 0)
    addr := addr | shiftBits(io.serial.in.bits, addrIdx)
    idx := idx + 1.U
    when (idx === (nChunksPerWord - 1).U) {
      idx := 0.U
      state := s_len
    }
  }

  when (state === s_len && io.serial.in.valid) {
    val lenIdx = idx(log2Up(nChunksPerWord) - 1, 0)
    len := len | shiftBits(io.serial.in.bits, lenIdx)
    idx := idx + 1.U
    when (idx === (nChunksPerWord - 1).U) {
      idx := addrToIdx(addr)
      when (cmd === cmd_write) {
        bodyValid := 0.U
        state := s_write_body
      } .elsewhen (cmd === cmd_read) {
        state := s_read_req
      } .otherwise {
        assert(false.B, "Bad TSI command")
      }
    }
  }

  when (state === s_read_req && mem.a.ready) {
    state := s_read_data
  }

  when (state === s_read_data && mem.d.valid) {
    body := body.fromBits(mem.d.bits.data)
    idx := addrToIdx(addr)
    addr := nextAddr
    state := s_read_body
  }

  when (state === s_read_body && io.serial.out.ready) {
    idx := idx + 1.U
    len := len - 1.U
    when (len === 0.U) { state := s_cmd }
    .elsewhen (idx === (nChunksPerWord - 1).U) { state := s_read_req }
  }

  when (state === s_write_body && io.serial.in.valid) {
    body(idx) := io.serial.in.bits
    bodyValid := bodyValid | UIntToOH(idx)
    when (idx === (nChunksPerWord - 1).U || len === 0.U) {
      state := s_write_data
    } .otherwise {
      idx := idx + 1.U
      len := len - 1.U
    }
  }

  when (state === s_write_data && mem.a.ready) {
    state := s_write_ack
  }

  when (state === s_write_ack && mem.d.valid) {
    when (len === 0.U) {
      state := s_cmd
    } .otherwise {
      addr := nextAddr
      len := len - 1.U
      idx := 0.U
      bodyValid := 0.U
      state := s_write_body
    }
  }
}

class SimSerial(w: Int) extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val serial = Flipped(new SerialIO(w))
    val exit = Output(Bool())
  })
}

trait HasPeripherySerial extends HasSystemNetworks {
  implicit val p: Parameters

  val adapter = LazyModule(new SerialAdapter)
  fsb.node := adapter.node
}

trait HasPeripherySerialModuleImp extends LazyMultiIOModuleImp {
  implicit val p: Parameters
  val outer: HasPeripherySerial

  val serial = IO(new SerialIO(p(SerialInterfaceWidth)))
  val adapter = outer.adapter.module
  serial.out <> Queue(adapter.io.serial.out)
  adapter.io.serial.in <> Queue(serial.in)

  def connectSimSerial() = {
    val sim = Module(new SimSerial(p(SerialInterfaceWidth)))
    sim.io.clock := clock
    sim.io.reset := reset
    sim.io.serial <> serial
    sim.io.exit
  }
}
