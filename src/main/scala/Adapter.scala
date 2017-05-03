package testchipip

import scala.math.min
import chisel3._
import chisel3.util._
import diplomacy.LazyModule
import uncore.tilelink._
import uncore.tilelink2.{TLLegacy, TLHintHandler}
import uncore.devices.{DebugBusIO, ToAsyncDebugBus, NTiles}
import uncore.coherence.{MESICoherence, NullRepresentation}
import coreplex.{CoreplexRISCVPlatform, BankedL2Config, CacheBlockBytes}
import junctions._
import rocketchip._
import tile.XLen
import rocket.PAddrBits
import config.{Parameters, Field}
import _root_.util._

case object SerialInterfaceWidth extends Field[Int]

object AdapterParams {
  def apply(p: Parameters) = p.alterPartial({
    case NastiKey => NastiParameters(
      dataBits = 32,
      addrBits = 32,
      idBits = 12)
    case TLId => "SerialtoL2"
    case TLKey("SerialtoL2") =>
      TileLinkParameters(
        coherencePolicy = new MESICoherence(new NullRepresentation(p(NTiles))),
        nManagers = p(BankedL2Config).nBanks + 1 /* MMIO */,
        nCachingClients = 1,
        nCachelessClients = 1,
        maxClientXacts = 1,
        maxClientsPerPort = 1,
        maxManagerXacts = 8,
        dataBeats = (8 * p(CacheBlockBytes)) / p(XLen),
        dataBits = p(CacheBlockBytes)*8)
    case CacheBlockOffsetBits => log2Ceil(p(CacheBlockBytes))
    case AmoAluOperandBits => p(XLen)
  })
}

class SerialAdapter(implicit p: Parameters) extends TLModule()(p) {
  val w = p(SerialInterfaceWidth)
  val io = IO(new Bundle {
    val serial = new SerialIO(w)
    val mem = new ClientUncachedTileLinkIO
  })

  val nChunksPerBeat = tlDataBits / w
  val pAddrBits = p(PAddrBits)
  val xLen = p(XLen)
  val nChunksPerWord = xLen / w

  require(nChunksPerBeat > 0, s"Serial interface width must be <= TileLink width $tlDataBits")
  require(nChunksPerWord > 0, s"Serial interface width must be <= PAddrBits $pAddrBits")

  val cmd = Reg(UInt(w.W))
  val addr = Reg(UInt(xLen.W))
  val len = Reg(UInt(xLen.W))
  val body = Reg(Vec(nChunksPerBeat, UInt(w.W)))
  val bodyValid = Reg(UInt(nChunksPerBeat.W))
  val idx = Reg(UInt(log2Up(nChunksPerBeat).W))

  val (cmd_read :: cmd_write :: Nil) = Enum(2)
  val (s_cmd :: s_addr :: s_len ::
       s_read_req  :: s_read_data :: s_read_body :: 
       s_write_body :: s_write_data :: s_write_ack :: Nil) = Enum(9)
  val state = Reg(init = s_cmd)

  io.serial.in.ready := state.isOneOf(s_cmd, s_addr, s_len, s_write_body)
  io.serial.out.valid := state === s_read_body
  io.serial.out.bits := body(idx)

  val blockOffset = tlBeatAddrBits + tlByteAddrBits
  val blockAddr = addr(pAddrBits - 1, blockOffset)
  val beatAddr = addr(blockOffset - 1, tlByteAddrBits)
  val nextAddr = Cat(Cat(blockAddr, beatAddr) + 1.U, 0.U(tlByteAddrBits.W))

  val wmask = FillInterleaved(w/8, bodyValid)
  val addr_size = nextAddr - addr
  val len_size = Cat(len + 1.U, 0.U(log2Ceil(w/8).W))
  val raw_size = Mux(len_size < addr_size, len_size, addr_size)
  val rsize = MuxLookup(raw_size, log2Ceil(tlDataBytes).U,
    (0 until log2Ceil(tlDataBytes)).map(i => ((1 << i).U -> i.U)))

  val pow2size = PopCount(raw_size) === 1.U
  val byteAddr = Mux(pow2size, addr(tlByteAddrBits - 1, 0), 0.U)

  val put_acquire = Put(
    client_xact_id = 0.U,
    addr_block = blockAddr,
    addr_beat = beatAddr,
    data = body.asUInt,
    wmask = Some(wmask))

  val get_acquire = Get(
    client_xact_id = 0.U,
    addr_block = blockAddr,
    addr_beat = beatAddr,
    addr_byte = byteAddr,
    operand_size = rsize,
    alloc = true.B)

  io.mem.acquire.valid := state.isOneOf(s_write_data, s_read_req)
  io.mem.acquire.bits := Mux(state === s_write_data, put_acquire, get_acquire)
  io.mem.grant.ready := state.isOneOf(s_write_ack, s_read_data)

  def shiftBits(bits: UInt, idx: UInt): UInt =
    bits << Cat(idx, 0.U(log2Up(w).W))

  def addrToIdx(addr: UInt): UInt =
    addr(tlByteAddrBits - 1, log2Up(w/8))


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

  when (state === s_read_req && io.mem.acquire.ready) {
    state := s_read_data
  }

  when (state === s_read_data && io.mem.grant.valid) {
    body := body.fromBits(io.mem.grant.bits.data)
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

  when (state === s_write_data && io.mem.acquire.ready) {
    state := s_write_ack
  }

  when (state === s_write_ack && io.mem.grant.valid) {
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

trait PeripherySerial extends TopNetwork {
  implicit val p: Parameters

  val serLegacy = LazyModule(new TLLegacy()(AdapterParams(p)))
  l2.node := TLHintHandler()(serLegacy.node)
}

trait PeripherySerialBundle {
  implicit val p: Parameters

  val serial = new SerialIO(p(SerialInterfaceWidth))
}

trait PeripherySerialModule {
  implicit val p: Parameters
  val outer: PeripherySerial
  val io: PeripherySerialBundle

  val adapter = Module(new SerialAdapter()(AdapterParams(p)))
  outer.serLegacy.module.io.legacy <> adapter.io.mem
  io.serial.out <> Queue(adapter.io.serial.out)
  adapter.io.serial.in <> Queue(io.serial.in)
}

trait NoDebug {
  val coreplex: CoreplexRISCVPlatform
}

trait NoDebugModule {
  implicit val p: Parameters
  val outer: NoDebug
  val debugIO = Wire(new DebugBusIO)

  debugIO.req.valid := false.B
  debugIO.resp.ready := false.B
  outer.coreplex.module.io.debug <> ToAsyncDebugBus(debugIO)
}
