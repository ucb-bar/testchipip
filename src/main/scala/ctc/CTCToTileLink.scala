package testchipip.ctc

import chisel3._
import chisel3.util._
import testchipip.serdes._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.{Parameters, Field}

// a tl-client (master) device
// from outer: receives read and write requests in CTC
// to inner: sends read and write requests in TL
// from inner: receives read and write responses in TL
// to outer: sends read and write responses in CTC
class CTCToTileLink(sourceIds: Int = 1)(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLClientParameters(
    name = "ctc", sourceId = IdRange(0, sourceIds))))))

  lazy val module = new CTCToTileLinkModule(this)
}

class CTCToTileLinkModule(outer: CTCToTileLink) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val flit = new DecoupledFlitIO(CTC.INNER_WIDTH)
  })
  
  val (mem, edge) = outer.node.out(0)
  require (edge.manager.minLatency > 0)

  // constants
  val cmdLen = 2
  val lenLen = 16
  val wordLen = 64
  val pAddrBits = edge.bundle.addressBits
  val nChunksPerWord = wordLen / CTC.INNER_WIDTH
  val dataBits = mem.params.dataBits
  val beatBytes = dataBits / 8
  val nChunksPerBeat = dataBits / CTC.INNER_WIDTH
  val byteAddrBits = log2Ceil(beatBytes)

  val len = Reg(UInt(lenLen.W))
  val cmd = Reg(UInt(cmdLen.W))
  val addr = Reg(UInt(wordLen.W))
  val body = Reg(Vec(nChunksPerBeat, UInt(CTC.INNER_WIDTH.W)))
  val bodyValid = Reg(UInt(nChunksPerBeat.W))
  val ack = Reg(Bool())

  val beatAddr = addr(pAddrBits - 1, byteAddrBits)
  val nextAddr = Cat(beatAddr + 1.U, 0.U(byteAddrBits.W))

  val wmask = FillInterleaved(CTC.INNER_WIDTH/8, bodyValid)

  // tl requests
  val tl_read_req = edge.Get(
    fromSource = 0.U, toAddress = addr, lgSize = log2Ceil(beatBytes).U)._2
  val tl_write_req = edge.Put(
    fromSource = 0.U, toAddress = addr, lgSize = log2Ceil(beatBytes).U, 
    data = body.asUInt, mask = wmask)._2

  val (s_cmd :: s_addr :: s_r_req :: s_r_data :: s_r_ack :: s_r_body ::
    s_w_body :: s_w_data :: s_w_ack :: Nil) = Enum(9)
  val state = RegInit(s_cmd)
  val idx = Reg(UInt(log2Up(nChunksPerWord).W))

  // state-driven signals
  io.flit.in.ready := state.isOneOf(s_cmd, s_addr, s_w_body)
  io.flit.out.valid := state.isOneOf(s_r_ack, s_r_body, s_w_ack)
  val out_bits = Mux(state === s_r_ack, Cat(CTCCommand.read_ack, len), // read ack header
                          Mux(state === s_w_ack, Cat(CTCCommand.write_ack, 0.U(lenLen.W)), // write ack header
                          body(idx))) // data flit
  io.flit.out.bits := out_bits.asTypeOf(io.flit.out.bits)

  mem.a.valid := state.isOneOf(s_r_req, s_w_data)
  mem.a.bits := Mux(state === s_r_req, tl_read_req, tl_write_req)
  mem.b.ready := false.B
  mem.c.valid := false.B
  mem.d.ready := state.isOneOf(s_r_ack, s_w_ack)
  mem.e.valid := false.B

  when (state === s_cmd && io.flit.in.valid) {
    len := io.flit.in.bits.flit(lenLen - 1, 0) + 1.U(lenLen.W) // always pad +1 
    cmd := io.flit.in.bits.flit(cmdLen + lenLen - 1, lenLen)
    addr := 0.U
    idx := 0.U
    ack := false.B
    state := s_addr
  }

  when (state === s_addr && io.flit.in.valid) {
    // older flits are at higher indices
    addr := (addr << CTC.INNER_WIDTH) | io.flit.in.bits.flit
    idx := idx + 1.U
    when (idx === (nChunksPerWord - 1).U) {
      idx := 0.U
      when (cmd === CTCCommand.read_req) {
        state := s_r_req
      } .elsewhen (cmd === CTCCommand.write_req) {
        state := s_w_body
      } .otherwise {
        assert(false.B, "Bad CTC command")
      }
    }
  }

  // BEGIN: handling read requests
  // send read request to inner TL
  when (state === s_r_req && mem.a.ready) {
    state := s_r_data
  }
  // wait for read data from inner TL to arrive
  when (state === s_r_data && mem.d.valid) {
    body := mem.d.bits.data.asTypeOf(body)
    state := Mux(ack, s_r_ack, s_r_body) // if ack is not sent, send acknowledgement header first
  }
  when (state === s_r_ack && io.flit.out.ready) {
    ack := true.B // set ack flag to true
    state := s_r_body
  }
  when (state === s_r_body && io.flit.out.ready) {
    idx := idx + 1.U
    len := len - 1.U
    when (len === 0.U) { state := s_cmd } // fully finished handling a CTC R Request
    .elsewhen (idx === (nChunksPerBeat - 1).U) { state := s_r_req } // send next TL R Request
  }
  // END: handling read requests

  // BEGIN: handling write requests
  when (state === s_w_body && io.flit.in.valid) {
    body(idx) := io.flit.in.bits.asUInt
    bodyValid := bodyValid | UIntToOH(idx)
    when (idx === (nChunksPerBeat - 1).U || len === 0.U) {
      state := s_w_data
    } .otherwise {
      idx := idx + 1.U
      len := len - 1.U
    }
  }

  when (state === s_w_data && mem.a.ready) {
    state := s_w_ack
  }

  when (state === s_w_ack && mem.d.valid) {
    when (len === 0.U) {
      state := s_cmd
    } .otherwise {
      addr := addr + 1.U
      len := len - 1.U
      idx := 0.U
      bodyValid := 0.U
      state := s_w_body
    }
  }
  // END: handling write requests
}