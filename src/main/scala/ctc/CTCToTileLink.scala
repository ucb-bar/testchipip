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

  val len = Reg(UInt(lenLen.W))
  val ctc_len = Reg(UInt(lenLen.W))
  val cmd = Reg(UInt(cmdLen.W))
  val addr = Reg(UInt(wordLen.W))
  val tladdr = Reg(UInt(wordLen.W))
  val body = Reg(Vec(nChunksPerBeat, UInt(CTC.INNER_WIDTH.W)))
  val ack = Reg(Bool())

  val next_tl_addr = tladdr + beatBytes.U

  // tl requests
  val tl_read_req = edge.Get(
    fromSource = 0.U, toAddress = tladdr, lgSize = log2Ceil(beatBytes).U)._2
  val tl_write_req = edge.Put(
    fromSource = 0.U, toAddress = tladdr, lgSize = log2Ceil(beatBytes).U, data = body.asUInt)._2
  
  // ====== state machine ======
  val (s_cmd :: s_addr :: s_r_req :: s_r_data :: s_send_ack :: s_send_addr :: s_r_body ::
    s_w_body :: s_w_data :: s_w_wait :: Nil) = Enum(10)
  val state = RegInit(s_cmd)
  val idx = Reg(UInt(log2Up(Math.max(nChunksPerBeat, nChunksPerWord)).W))

  // state-driven signals
  io.flit.in.ready := state.isOneOf(s_cmd, s_addr, s_w_body)
  io.flit.out.valid := state.isOneOf(s_send_ack, s_send_addr, s_r_body)
  val out_bits = Mux(state === s_send_ack && cmd === CTCCommand.read_req, Cat(CTCCommand.read_ack, ctc_len), // read ack header
                  Mux(state === s_send_ack && cmd === CTCCommand.write_req, Cat(CTCCommand.write_ack, ctc_len), // write ack header
                  Mux(state === s_send_addr, addr(CTC.INNER_WIDTH - 1, 0), // send address header
                  body(idx)))) // data flit
  io.flit.out.bits := out_bits.asTypeOf(io.flit.out.bits)

  mem.a.valid := state.isOneOf(s_r_req, s_w_data)
  mem.a.bits := Mux(state === s_r_req, tl_read_req, tl_write_req)
  mem.b.ready := false.B
  mem.c.valid := false.B
  mem.d.ready := state.isOneOf(s_r_data, s_w_wait)
  mem.e.valid := false.B

  when (state === s_cmd && io.flit.in.valid) {
    len := io.flit.in.bits.flit(lenLen - 1, 0) + 1.U
    ctc_len := io.flit.in.bits.flit(lenLen - 1, 0)
    cmd := io.flit.in.bits.flit(cmdLen + lenLen - 1, lenLen)
    addr := 0.U
    idx := 0.U
    ack := false.B
    state := s_addr
    body.foreach(_ := 0.U)
  }

  when (state === s_addr && io.flit.in.valid) {
    // older flits are at higher indices
    addr := addr | (io.flit.in.bits.flit << (idx * CTC.INNER_WIDTH.U))
    idx := idx + 1.U
    when (idx === (nChunksPerWord - 1).U) {
      idx := 0.U
      tladdr := addr
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
    state := Mux(~ack, s_send_ack, s_r_body) // if ack is not sent, send acknowledgement header first
  }
  // send the read ack to outer CTC if this is the first beat
  when (state === s_send_ack && io.flit.out.ready) {
    ack := true.B // set ack flag to true
    idx := 0.U
    state := s_send_addr
  }
  // send the address header to outer CTC
  when (state === s_send_addr && io.flit.out.ready) {
    idx := idx + 1.U
    addr := addr >> CTC.INNER_WIDTH.U
    when (idx === (nChunksPerWord - 1).U) {
      state := Mux(cmd === CTCCommand.read_req, s_r_body, s_cmd)
    }
  }
  // send the read data to outer CTC
  when (state === s_r_body && io.flit.out.ready) {
    idx := idx + 1.U
    len := len - 1.U
    when (idx === (nChunksPerBeat - 1).U || len === 1.U) { 
      tladdr := next_tl_addr
      state := Mux(len === 1.U, s_cmd, s_r_req) // send next TL R Request
    } 
  }
  // END: handling read requests

  // BEGIN: handling write requests
  // collect the write data from the CTC
  when (state === s_w_body && io.flit.in.valid) {
    body(idx) := io.flit.in.bits.asUInt
    len := len - 1.U
    when (idx === (nChunksPerBeat - 1).U || len === 1.U) {
      state := s_w_data
    } .otherwise {
      idx := idx + 1.U
    }
  }

  // send the write request to inner TL
  when (state === s_w_data && mem.a.ready) {
    state := s_w_wait // wait for write response from inner TL
  }

  // wait for write response from inner TL
  when (state === s_w_wait && mem.d.valid) {
    when (len === 0.U) { // am I the last beat?
      state := s_send_ack
    } .otherwise {
      // addr := addr + 1.U
      tladdr := next_tl_addr
      idx := 0.U
      state := s_w_body
    }
  }
  // END: handling write requests
}