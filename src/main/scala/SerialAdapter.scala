package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem.{BaseSubsystem}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.min

case object SerialAdapter {
  val SERIAL_IF_WIDTH = 32
}

class SerialAdapter(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "serial", sourceId = IdRange(0,1))

  lazy val module = new LazyModuleImp(this) {
    val (mem, edge) = outer.node.out(0)

    val io = IO(new Bundle {
      val serial = new SerialIO(SerialAdapter.SERIAL_IF_WIDTH)
    })

    // We only expect one client
    require(edge.clients.size == 1)
    // We expect that client to only have one ID
    require(edge.clients(0).sourceId.size == 1)
    val serialAdapter = Module(new SerialAdapterModule(edge.bundle, edge.clients(0).sourceId.start))

    mem <> serialAdapter.io.clientTL
    io.serial <> serialAdapter.io.serial
  }
}

class SerialAdapterModule(clientParams: TLBundleParameters, sourceId: Int) extends Module {

  val io = IO(new Bundle {
    val serial = new SerialIO(SerialAdapter.SERIAL_IF_WIDTH)
    val clientTL = new TLBundle(clientParams)
  })

  val pAddrBits = clientParams.addressBits
  val wordLen = 64
  val nChunksPerWord = wordLen / w
  val dataBits = clientParams.dataBits
  val beatBytes = dataBits / 8
  val nChunksPerBeat = dataBits / w
  val byteAddrBits = log2Ceil(beatBytes)

  require(nChunksPerWord > 0, s"Serial interface width must be <= $wordLen")

  val cmd = Reg(UInt(w.W))
  val addr = Reg(UInt(wordLen.W))
  val len = Reg(UInt(wordLen.W))
  val body = Reg(Vec(nChunksPerBeat, UInt(w.W)))
  val bodyValid = Reg(UInt(nChunksPerBeat.W))
  val idx = Reg(UInt(log2Up(nChunksPerBeat).W))

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
    (0 until log2Ceil(beatBytes)).map(i => ((1 << i).U -> i.U)))

  val pow2size = PopCount(raw_size) === 1.U
  val byteAddr = Mux(pow2size, addr(byteAddrBits - 1, 0), 0.U)

  val put_acquire = edge.Put(
    0.U, beatAddr << byteAddrBits.U, log2Ceil(beatBytes).U,
    body.asUInt, wmask)._2

  val get_acquire = edge.Get(
    0.U, Cat(beatAddr, byteAddr), rsize)._2

  io.clientTL.a.valid := state.isOneOf(s_write_data, s_read_req)
  io.clientTL.a.bits := Mux(state === s_write_data, put_acquire, get_acquire)
  io.clientTL.b.ready := false.B
  io.clientTL.c.valid := false.B
  io.clientTL.d.ready := state.isOneOf(s_write_ack, s_read_data)
  io.clientTL.e.valid := false.B

  def shiftBits(bits: UInt, idx: UInt): UInt =
    if (nChunksPerWord > 1)
      bits << Cat(idx(log2Ceil(nChunksPerWord) - 1, 0), 0.U(log2Up(w).W))
    else bits

  def addrToIdx(addr: UInt): UInt =
    if (nChunksPerBeat > 1) addr(byteAddrBits - 1, log2Up(w/8)) else 0.U

  when (state === s_cmd && io.serial.in.valid) {
    cmd := io.serial.in.bits
    idx := 0.U
    addr := 0.U
    len := 0.U
    state := s_addr
  }

  when (state === s_addr && io.serial.in.valid) {
    addr := addr | shiftBits(io.serial.in.bits, idx)
    idx := idx + 1.U
    when (idx === (nChunksPerWord - 1).U) {
      idx := 0.U
      state := s_len
    }
  }

  when (state === s_len && io.serial.in.valid) {
    len := len | shiftBits(io.serial.in.bits, idx)
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

  when (state === s_read_req && io.clientTL.a.ready) {
    state := s_read_data
  }

  when (state === s_read_data && io.clientTL.d.valid) {
    body := io.clientTL.d.bits.data.asTypeOf(body)
    idx := addrToIdx(addr)
    addr := nextAddr
    state := s_read_body
  }

  when (state === s_read_body && io.serial.out.ready) {
    idx := idx + 1.U
    len := len - 1.U
    when (len === 0.U) { state := s_cmd }
    .elsewhen (idx === (nChunksPerBeat - 1).U) { state := s_read_req }
  }

  when (state === s_write_body && io.serial.in.valid) {
    body(idx) := io.serial.in.bits
    bodyValid := bodyValid | UIntToOH(idx)
    when (idx === (nChunksPerBeat - 1).U || len === 0.U) {
      state := s_write_data
    } .otherwise {
      idx := idx + 1.U
      len := len - 1.U
    }
  }

  when (state === s_write_data && io.clientTL.a.ready) {
    state := s_write_ack
  }

  when (state === s_write_ack && io.clientTL.d.valid) {
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

trait HasPeripherySerial { this: BaseSubsystem =>
  private val portName = "serial-adapter"
  val adapter = LazyModule(new SerialAdapter)
  sbus.fromPort(Some(portName))() := adapter.node
}

trait HasPeripherySerialModuleImp extends LazyModuleImp {
  implicit val p: Parameters
  val outer: HasPeripherySerial

  val serial = IO(new SerialIO(SerialAdapter.SERIAL_IF_WIDTH))
  val adapter = outer.adapter.module
  serial.out <> Queue(adapter.io.serial.out)
  adapter.io.serial.in <> Queue(serial.in)

  def connectSimSerial() = {
    val sim = Module(new SimSerial(SerialAdapter.SERIAL_IF_WIDTH))
    sim.io.clock := clock
    sim.io.reset := reset
    sim.io.serial <> serial
    sim.io.exit
  }
}
