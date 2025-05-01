package testchipip.ctc

import chisel3._
import chisel3.util._
import testchipip.serdes._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.{Parameters, Field}

// a tl-manager (slave) device
// from inner: receives read and write requests in TL
// to outer: sends read and write requests in CTC
// from outer: receives read and write responses in CTC
// to inner: sends read and write responses in TL
class TileLinkToCTC(sinkIds: Int = 1, val beatBytes: Int = 8, baseAddr: BigInt = 0, size: BigInt = 1024, val maxBeats: Int = 4)
                  (implicit p: Parameters) extends LazyModule {
  val addrSet = AddressSet(baseAddr, size - 1)
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v2(
      address = Seq(addrSet),
      regionType = RegionType.UNCACHED,
      supports = TLMasterToSlaveTransferSizes(
        putFull = TransferSizes(1, beatBytes*maxBeats), 
        get = TransferSizes(1, beatBytes*maxBeats)
      ),
      fifoId = Some(0))), // requests are handled in order
    beatBytes = beatBytes)))

  lazy val module = new TileLinkToCTCModule(this)
}

class TileLinkToCTCModule(outer: TileLinkToCTC) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val flit = new DecoupledFlitIO(CTC.INNER_WIDTH)
  })

  val (mem, edge) = outer.node.in(0)
  dontTouch(mem.d.bits.size)

  // constants
  val cmdLen = 2
  val lenLen = 16
  val wordLen = 64
  val dataBits = mem.params.dataBits
  val beatBytes = dataBits / 8
  assert(beatBytes == outer.beatBytes, "Beat bytes mismatch")
  val nChunksPerBeat = dataBits / CTC.INNER_WIDTH
  val byteAddrBits = log2Ceil(beatBytes)
  val nChunksPerWord = wordLen / CTC.INNER_WIDTH

  val len = Reg(UInt(lenLen.W))
  val ctc_len = Reg(UInt(lenLen.W))
  val a_lg_size = Reg(UInt(log2Ceil(beatBytes * outer.maxBeats).W))
  val a_source = Reg(UInt(edge.bundle.sourceBits.W))
  dontTouch(a_lg_size)
  val cmd = Reg(UInt(cmdLen.W))
  val addr = Reg(UInt(wordLen.W))
  val body = Reg(Vec(nChunksPerBeat, UInt(CTC.INNER_WIDTH.W)))

  // ====== state machine ======
  val (s_idle :: s_send_cmd :: s_send_addr :: s_recv_cmd:: s_recv_addr :: 
    s_r_body :: s_r_ack :: s_w_req :: s_w_body :: s_w_ack ::s_reject :: Nil) = Enum(11)
  val state = RegInit(s_idle)
  val idx = Reg(UInt(log2Up(Math.max(nChunksPerBeat, nChunksPerWord)).W))

  // state-driven signals
  io.flit.in.ready := state.isOneOf(s_r_body, s_recv_cmd, s_recv_addr)
  io.flit.out.valid:= state.isOneOf(s_send_cmd, s_send_addr, s_w_body)
  val out_bits = Mux(state === s_send_cmd, Cat(cmd, len - 1.U),
    Mux(state === s_send_addr, addr(CTC.INNER_WIDTH - 1, 0), body(idx)))
  io.flit.out.bits := out_bits.asTypeOf(io.flit.out.bits)

  val tl_write_ack = edge.AccessAck(a_source, a_lg_size)
  val tl_read_ack = edge.AccessAck(a_source, a_lg_size, body(idx).asUInt)

  mem.a.ready := state.isOneOf(s_idle, s_w_req)
  mem.b.valid := false.B
  mem.c.ready := false.B
  mem.d.valid := state.isOneOf(s_r_ack, s_w_ack)
  mem.d.bits := Mux(state === s_r_ack, tl_read_ack, tl_write_ack)
  mem.e.ready := false.B


  when (state === s_idle && mem.a.valid) {
    len := (1.U << mem.a.bits.size) / CTC.INNER_WIDTH_BYTES.U
    ctc_len := (1.U << mem.a.bits.size) / CTC.INNER_WIDTH_BYTES.U
    a_lg_size := mem.a.bits.size
    a_source := mem.a.bits.source
    cmd := Mux(mem.a.bits.opcode === TLMessages.Get, CTCCommand.read_req, CTCCommand.write_req)
    addr := mem.a.bits.address
    state := Mux(mem.a.bits.size >= 2.U, s_send_cmd, s_reject)
    // state := s_send_cmd
    idx := 0.U
    when (cmd === CTCCommand.write_req) { body := mem.a.bits.data.asTypeOf(body) }
  }

  when (state === s_send_cmd && io.flit.out.ready) {
    state := s_send_addr
  }

  when (state === s_send_addr && io.flit.out.ready) {
    addr := (addr >> CTC.INNER_WIDTH) 
    idx := idx + 1.U
    when (idx === (nChunksPerWord - 1).U) {
      idx := 0.U
      state := Mux(cmd === CTCCommand.read_req, s_recv_cmd, s_w_body)
    }
  }

  when (state === s_recv_cmd && io.flit.in.valid) {
    assert((io.flit.in.bits.flit(lenLen - 1, 0) === ctc_len - 1.U), "Mismatch in CTC length")
    val ack_cmd = Mux(cmd === CTCCommand.read_req, CTCCommand.read_ack, CTCCommand.write_ack)
    assert(io.flit.in.bits.flit(cmdLen + lenLen - 1, lenLen) === ack_cmd, "Mismatch in CTC command")
    state := s_recv_addr
  } 

  when (state === s_recv_addr && io.flit.in.valid) {
    // assert(io.flit.in.bits.flit === ((addr >> (idx * CTC.INNER_WIDTH.U)) & ((1.U << CTC.INNER_WIDTH.U) - 1.U)), "Mismatch in CTC address")
    when (idx === (nChunksPerWord - 1).U) {
      idx := 0.U
      state := Mux(cmd === CTCCommand.read_req, s_r_body, s_w_ack)
    } .otherwise {
      idx := idx + 1.U
    }
  }

  // BEGIN: handling read requests
  // wait and collect the read response from CTC
  when (state === s_r_body && io.flit.in.valid) {
    body(idx) := io.flit.in.bits.flit.asUInt
    idx := idx + 1.U
    len := len - 1.U
    when (idx === nChunksPerBeat.U - 1.U || len === 1.U) {
      state := s_r_ack
    }
  }
  // send the read acknoledgement to TL
  when (state === s_r_ack && mem.d.ready) {
    state := s_idle
    when (len === 0.U) {
      state := s_idle
    } .otherwise {
      idx := 0.U
      state := s_r_body
    }
  }
  // END: handling read requests

  // BEGIN: handling write requests
  // send the write data to CTC
  when (state === s_w_body && io.flit.out.ready) {
    idx := idx + 1.U
    len := len - 1.U
    when (idx === nChunksPerBeat.U - 1.U || len === 1.U) {
      idx := 0.U
      state := Mux(len === 1.U, s_recv_cmd, s_w_req)
    }
  }

  // receive a new write beat from TL
  when (state === s_w_req && mem.a.ready) {
    state := s_w_body
    body := mem.a.bits.data.asTypeOf(body)
  }

  // send the write acknoledgement to TL
  when (state === s_w_ack && mem.d.ready) {
    state := s_idle
  }
  // END: handling write requests

  // BEGIN: reject sub-word transactions
  when (state === s_reject && mem.d.ready) {
    // TODO: send reject to TL D channel
    state := s_idle
  }
  // END: reject sub-word transactions
}