package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.config.{Parameters}

/*
 * RoCC "accelerator" which translates a virtual address
 * Useful for giving user programs info about physical addresses
 */
class PTWProber(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes, 1) {
  val nTrackers = 8

  override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1("PTWProber", sourceId = IdRange(0,nTrackers)))
  )))

  override lazy val module = new LazyRoCCModuleImp(this) with HasCoreParameters {
    val in_q = Queue(io.cmd)
    val translate_in_flight = RegInit(false.B)

    class RoCCBundle extends Bundle {
      val cmd = new RoCCCommand
      val data = UInt(64.W)
    }

    val resp_q = Module(new Queue(new RoCCBundle, 2))

    val vm_enabled = usingVM.B && io.ptw(0).ptbr.mode(io.ptw(0).ptbr.mode.getWidth-1) && in_q.bits.status.dprv <= PRV.S.U

    io.ptw(0).req.valid := in_q.valid && resp_q.io.enq.ready && !translate_in_flight && vm_enabled
    io.ptw(0).req.bits.valid := in_q.valid
    io.ptw(0).req.bits.bits.addr := in_q.bits.rs1(vaddrBits-1, pgIdxBits)
    io.ptw(0).req.bits.bits.vstage1 := false.B
    io.ptw(0).req.bits.bits.stage2 := false.B
    io.ptw(0).req.bits.bits.need_gpa := false.B

    when (io.ptw(0).req.fire()) {
      translate_in_flight := true.B
    } .elsewhen (io.ptw(0).resp.valid) {
      translate_in_flight := false.B
    }
    in_q.ready := Mux(translate_in_flight, io.ptw(0).resp.valid, !vm_enabled && resp_q.io.enq.ready)


    resp_q.io.enq.valid := Mux(translate_in_flight, io.ptw(0).resp.valid, !vm_enabled && in_q.valid)
    resp_q.io.enq.bits.cmd := in_q.bits
    resp_q.io.enq.bits.data := Mux(translate_in_flight,
      Mux(io.ptw(0).resp.bits.pf, 0.U, Cat(io.ptw(0).resp.bits.pte.ppn, in_q.bits.rs1(pgIdxBits-1,0))),
      in_q.bits.rs1)

    dontTouch(io.ptw(0).resp)

    val flush_trackers = RegInit(VecInit(0.U(nTrackers.W).asBools))
    val flush_ready = !flush_trackers.reduce(_&&_)
    val (tl_out, edge) = atlNode.out(0)

    io.resp.valid := resp_q.io.deq.valid && resp_q.io.deq.bits.cmd.inst.funct === 0.U
    io.resp.bits.rd := resp_q.io.deq.bits.cmd.inst.rd
    io.resp.bits.data := resp_q.io.deq.bits.data
    resp_q.io.deq.ready := Mux(resp_q.io.deq.bits.cmd.inst.funct === 0.U,
      io.resp.ready,
      flush_ready && tl_out.a.ready)

    tl_out.a.valid := resp_q.io.deq.valid && resp_q.io.deq.bits.cmd.inst.funct =/= 0.U && flush_ready
    tl_out.a.bits := edge.Put(
      fromSource = PriorityEncoder(~(flush_trackers.asUInt)),
      toAddress = 0x2010200.U,
      lgSize = log2Ceil(8).U,
      data = resp_q.io.deq.bits.data,
      corrupt = false.B
    )._2
    when (tl_out.a.fire()) {
      flush_trackers(tl_out.a.bits.source) := true.B
    }
    tl_out.d.ready := true.B
    when (tl_out.d.fire()) {
      flush_trackers(tl_out.d.bits.source) := false.B
    }

    tl_out.b.ready := false.B
    tl_out.c.valid := false.B
    tl_out.c.bits := DontCare
    tl_out.e.valid := false.B
    tl_out.e.bits := DontCare

    io.mem := DontCare
    io.mem.req.valid := false.B
    io.fpu_req.valid := false.B
    io.fpu_req.bits := DontCare
    io.fpu_resp.ready := false.B
    io.busy := in_q.valid || resp_q.io.deq.valid || flush_trackers.reduce(_||_)
    io.interrupt := false.B
  }
}
