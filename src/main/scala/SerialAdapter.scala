package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.min

case object SerialAdapter {
  val SERIAL_IF_WIDTH = 32
}
import SerialAdapter._

class SerialAdapter(sourceIds: Int = 1)(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "serial", sourceId = IdRange(0, sourceIds))

  lazy val module = new SerialAdapterModule(this)
}

class SerialAdapterModule(outer: SerialAdapter) extends LazyModuleImp(outer) {
  val w = SERIAL_IF_WIDTH
  val io = IO(new Bundle {
    val serial = new SerialIO(w)
  })

  val (mem, edge) = outer.node.out(0)

  val pAddrBits = edge.bundle.addressBits
  val wordLen = 64
  val chunkBytes = w/8
  val blockBytes = p(CacheBlockBytes)
  val nChunksPerWord = wordLen / w
  val dataBits = mem.params.dataBits
  val beatBytes = dataBits / 8
  val nChunksPerBeat = dataBits / w
  val nBeatsPerBlock = blockBytes / beatBytes
  val nChunksPerBlock = nBeatsPerBlock * nChunksPerBeat
  val blockOffset = log2Ceil(blockBytes)
  val beatOffset = log2Ceil(beatBytes)
  val beatAddrBits = log2Ceil(nBeatsPerBlock)
  val chunkAddrBits = log2Ceil(nChunksPerBeat)
  val byteAddrBits = log2Ceil(chunkBytes)

  require(nChunksPerWord > 0, s"Serial interface width must be <= $wordLen")

  val cmd = Reg(UInt(w.W))
  val addr = Reg(UInt(wordLen.W))
  val len = Reg(UInt(wordLen.W))
  val body = Reg(Vec(nBeatsPerBlock, Vec(nChunksPerBeat, UInt(w.W))))
  val bodyLen = Reg(UInt(log2Ceil(nChunksPerBlock+1).W))
  val bodyValid = Reg(UInt(nChunksPerBlock.W))

  val idxBits = beatAddrBits + chunkAddrBits
  val writeIdx = Reg(UInt(idxBits.W))
  val readIdx = Reg(UInt(idxBits.W))

  val writeChunk = writeIdx(chunkAddrBits - 1, 0)
  val readChunk  = readIdx( chunkAddrBits - 1, 0)
  val writeBeat  = writeIdx >> chunkAddrBits.U
  val readBeat   = readIdx  >> chunkAddrBits.U

  val addrIdx = addr(blockOffset-1, byteAddrBits)
  val addrBeat = addr(blockOffset-1, beatOffset)

  val (cmd_read :: cmd_write :: Nil) = Enum(2)
  val (s_cmd :: s_addr :: s_len ::
       s_read_req  :: s_read_data :: s_read_body :: 
       s_write_body :: s_write_data :: s_write_ack :: Nil) = Enum(9)
  val state = RegInit(s_cmd)

  io.serial.in.ready := state.isOneOf(s_cmd, s_addr, s_len, s_write_body)
  io.serial.out.valid := state === s_read_body
  io.serial.out.bits := body(readBeat)(readChunk)

  val blockAddr = addr(pAddrBits - 1, blockOffset)
  val nextAddr = Cat(blockAddr + 1.U, 0.U(blockOffset.W))

  val lenBytes = Cat(len + 1.U, 0.U(byteAddrBits.W))
  val bodyLenBytes = Cat(bodyLen, 0.U(byteAddrBits.W))
  val partial = lenBytes <= beatBytes.U
  val beatChunksValid = (bodyValid >> readIdx)(nChunksPerBeat-1, 0)

  val rsize = MuxCase(beatOffset.U, (0 until beatOffset).map(
    i => (lenBytes <= (1 << i).U) -> i.U))
  val wsize = MuxCase(blockOffset.U, (0 until blockOffset).map(
    i => (addr(i) || bodyLenBytes <= (1 << i).U) -> i.U))
  val wmask = FillInterleaved(w/8, beatChunksValid)

  val putAcquire = edge.Put(
    fromSource = 0.U,
    toAddress = addr,
    lgSize = wsize,
    data = body(readBeat).asUInt,
    mask = wmask)._2

  val getAcquire = edge.Get(
    fromSource = 0.U,
    toAddress = Mux(partial, addr, Cat(blockAddr, 0.U(blockOffset.W))),
    lgSize = Mux(partial, rsize, blockOffset.U))._2

  mem.a.valid := state.isOneOf(s_write_data, s_read_req)
  mem.a.bits := Mux(state === s_write_data, putAcquire, getAcquire)
  mem.b.ready := false.B
  mem.c.valid := false.B
  mem.d.ready := state.isOneOf(s_write_ack, s_read_data)
  mem.e.valid := false.B

  def shiftBits(bits: UInt, idx: UInt): UInt =
    if (nChunksPerWord > 1)
      bits << Cat(idx(log2Ceil(nChunksPerWord) - 1, 0), 0.U(log2Up(w).W))
    else bits

  when (state === s_cmd && io.serial.in.valid) {
    cmd := io.serial.in.bits
    writeIdx := 0.U
    addr := 0.U
    len := 0.U
    state := s_addr
  }

  when (state === s_addr && io.serial.in.valid) {
    addr := addr | shiftBits(io.serial.in.bits, writeIdx)
    writeIdx := writeIdx + 1.U
    when (writeIdx === (nChunksPerWord - 1).U) {
      writeIdx := 0.U
      state := s_len
    }
  }

  when (state === s_len && io.serial.in.valid) {
    len := len | shiftBits(io.serial.in.bits, writeIdx)
    when (writeIdx === (nChunksPerWord - 1).U) {
      when (cmd === cmd_write) {
        writeIdx := addrIdx
        bodyLen := 0.U
        bodyValid := 0.U
        state := s_write_body
      } .elsewhen (cmd === cmd_read) {
        state := s_read_req
      } .otherwise {
        assert(false.B, "Bad TSI command")
      }
    } .otherwise {
      writeIdx := writeIdx + 1.U
    }
  }

  when (state === s_read_req && mem.a.ready) {
    bodyLen := 0.U
    writeIdx := Mux(partial, addrIdx, 0.U)
    state := s_read_data
  }

  when (state === s_read_data && mem.d.valid) {
    body(writeBeat) := (0 until nChunksPerBeat).map(
      i => mem.d.bits.data(w * (i + 1) - 1, w * i))
    writeIdx := writeIdx + nChunksPerBeat.U
    bodyLen := bodyLen + nChunksPerBeat.U

    when (edge.last(mem.d)) {
      readIdx := addrIdx
      state := s_read_body
    }
  }

  when (state === s_read_body && io.serial.out.ready) {
    readIdx := readIdx + 1.U
    bodyLen := bodyLen - 1.U

    when (len === 0.U) {
      state := s_cmd
    } .otherwise {
      len := len - 1.U
      when (bodyLen === 1.U || readIdx === (nChunksPerBlock-1).U) {
        addr := nextAddr
        state := s_read_req
      }
    }
  }

  when (state === s_write_body && io.serial.in.valid) {
    body(writeBeat)(writeChunk) := io.serial.in.bits
    bodyValid := bodyValid | UIntToOH(writeIdx)
    bodyLen := bodyLen + 1.U
    when (writeIdx === (nChunksPerBlock - 1).U || len === 0.U) {
      readIdx := Cat(addrBeat, 0.U(chunkAddrBits.W))
      state := s_write_data
    } .otherwise {
      len := len - 1.U
      writeIdx := writeIdx + 1.U
    }
  }

  when (state === s_write_data && mem.a.ready) {
    readIdx := readIdx + nChunksPerBeat.U
    when (edge.last(mem.a)) {
      addr := addr + (1.U << wsize)
      state := s_write_ack
    }
  }

  when (state === s_write_ack && mem.d.valid) {
    when (len === 0.U) {
      state := s_cmd
    } .otherwise {
      bodyValid := 0.U
      bodyLen := 0.U
      len := len - 1.U
      writeIdx := addrIdx
      state := s_write_body
    }
  }
}

class SimSerial(w: Int) extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val serial = Flipped(new SerialIO(w))
    val exit = Output(Bool())
  })

  setResource("/testchipip/vsrc/SimSerial.v")
  setResource("/testchipip/csrc/SimSerial.cc")
}

trait HasPeripherySerial { this: BaseSubsystem =>
  private val portName = "serial-adapter"
  val adapter = LazyModule(new SerialAdapter)
  sbus.fromPort(Some(portName))() := adapter.node
}

trait HasPeripherySerialModuleImp extends LazyModuleImp {
  implicit val p: Parameters
  val outer: HasPeripherySerial

  val serial = IO(new SerialIO(SERIAL_IF_WIDTH))
  val adapter = outer.adapter.module
  serial.out <> Queue(adapter.io.serial.out)
  adapter.io.serial.in <> Queue(serial.in)

  def connectSimSerial() = {
    val sim = Module(new SimSerial(SERIAL_IF_WIDTH))
    sim.io.clock := clock
    sim.io.reset := reset
    sim.io.serial <> serial
    sim.io.exit
  }
}
