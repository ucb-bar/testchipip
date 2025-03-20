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
class TileLinkToCTC(sinkIds: Int = 1, beatBytes: Int = 8, baseAddr: BigInt = 0, size: BigInt = 1024)
                  (implicit p: Parameters) extends LazyModule {
  val addrSet = AddressSet(baseAddr, size - 1)
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v2(
                  address = Seq(addrSet),
                  )),
    beatBytes = beatBytes)))

  lazy val module = new TileLinkToCTCModule(this)
}

class TileLinkToCTCModule(outer: TileLinkToCTC) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val flit = new DecoupledFlitIO(CTC.INNER_WIDTH)
  })

  val (mem, edge) = outer.node.in(0)

  // constants
  val cmdLen = 2
  val lenLen = 16
  val wordLen = 64
  val dataBits = mem.params.dataBits
  val beatBytes = dataBits / 8
  val nChunksPerBeat = dataBits / CTC.INNER_WIDTH
  val byteAddrBits = log2Ceil(beatBytes)
  val nChunksPerWord = wordLen / CTC.INNER_WIDTH

  val len = Reg(UInt(lenLen.W))
  val cmd = Reg(UInt(cmdLen.W))
  val addr = Reg(UInt(wordLen.W))
  val body = Reg(Vec(nChunksPerBeat, UInt(CTC.INNER_WIDTH.W)))

  val (s_idle :: s_cmd :: s_addr :: s_r_body :: s_r_ack :: Nil) = Enum(6)
  val state = RegInit(s_idle)
  val idx = Reg(UInt(log2Up(nChunksPerWord).W))

  // state-driven signals
  io.flit.in.ready := state.isOneOf(s_r_body)
  io.flit.out.valid:= state.isOneOf(s_cmd, s_addr)
  io.flit.out.bits := Mux(state === s_cmd, Cat(cmd, len),
    Mux(state === s_addr, addr(CTC.INNER_WIDTH - 1, 0), body(idx))) // TODO: add the rest of the data here

  mem.a.ready := state === s_idle
  mem.b.valid := false.B
  mem.c.ready := false.B
  mem.d.valid := state === s_r_ack 
  mem.e.ready := false.B

  when (state === s_idle && mem.a.valid) {
    len := mem.a.bits.size
    cmd := Mux(mem.a.bits.opcode === TLMessages.Get, CTCCommand.read_req, CTCCommand.write_req)
    addr := mem.a.bits.address
    state := s_cmd
  }

  when (state === s_cmd && io.flit.out.ready) {
    state := s_addr
  }

  when (state === s_addr && io.flit.out.ready) {
    addr := (addr >> CTC.INNER_WIDTH) 
    idx := idx + 1.U
    when (idx === (nChunksPerWord - 1).U) {
      idx := 0.U
      state := s_r_body
    }
  }

  when (state === s_r_body && io.flit.in.valid) {
    body(idx) := io.flit.in.bits.flit
    idx := idx + 1.U
    when (idx === len - 1.U) {
      idx := 0.U
      state := s_r_ack
    }
  }

  when (state === s_r_ack && mem.d.ready) {
    state := s_idle
  } 
}