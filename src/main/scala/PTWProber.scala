package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._
import freechips.rocketchip.config.{Parameters}

/*
 * RoCC "accelerator" which translates a virtual address
 * Useful for giving user programs info about physical addresses
 */
class PTWProber(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes, 1) {
  override lazy val module = new LazyRoCCModuleImp(this) with HasCoreParameters {
    val in_q = Queue(io.cmd)
    val in_flight = RegInit(false.B)
    val resp_q = Module(new Queue(new RoCCResponse, 2))

    val vm_enabled = usingVM.B && io.ptw(0).ptbr.mode(io.ptw(0).ptbr.mode.getWidth-1) && in_q.bits.status.dprv <= PRV.S.U

    io.ptw(0).req.valid := in_q.valid && resp_q.io.enq.ready && !in_flight && vm_enabled
    io.ptw(0).req.bits.valid := in_q.valid
    io.ptw(0).req.bits.bits.addr := in_q.bits.rs1(vaddrBits-1, pgIdxBits)
    io.ptw(0).req.bits.bits.vstage1 := false.B
    io.ptw(0).req.bits.bits.stage2 := false.B
    io.ptw(0).req.bits.bits.need_gpa := false.B

    when (io.ptw(0).req.fire()) {
      in_flight := true.B
    } .elsewhen (io.ptw(0).resp.valid) {
      in_flight := false.B
    }
    in_q.ready := Mux(in_flight, io.ptw(0).resp.valid, !vm_enabled && resp_q.io.enq.ready)


    resp_q.io.enq.valid := Mux(in_flight, io.ptw(0).resp.valid, !vm_enabled && in_q.valid)
    resp_q.io.enq.bits.rd := in_q.bits.inst.rd
    resp_q.io.enq.bits.data := Mux(in_flight,
      Mux(io.ptw(0).resp.bits.pf, 0.U, Cat(io.ptw(0).resp.bits.pte.ppn, in_q.bits.rs1(pgIdxBits-1,0))),
      in_q.bits.rs1)

    dontTouch(io.ptw(0).resp)

    io.resp <> resp_q.io.deq

    io.fpu_req.valid := false.B
    io.fpu_req.bits := DontCare
    io.fpu_resp.ready := false.B
    io.mem := DontCare
    io.mem.req.valid := false.B
    io.busy := in_q.valid || resp_q.io.deq.valid
    io.interrupt := false.B
  }
}
