package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.subsystem.{BaseSubsystem, HasTiles}
import freechips.rocketchip.config.{Field, Config}
import freechips.rocketchip.diplomacy.{LazyModule, AddressSet, LazyModuleImp, BundleBridgeNexus}
import freechips.rocketchip.tilelink.{TLRAM}
import freechips.rocketchip.rocket.TracedInstruction
import freechips.rocketchip.util._
import freechips.rocketchip.config.Parameters

case class TracedInstructionWidths(iaddr: Int, insn: Int, wdata: Int, cause: Int, tval: Int)

object TracedInstructionWidths {
  def apply(tI: ExtendedTracedInstruction): TracedInstructionWidths =
    TracedInstructionWidths(tI.iaddr.getWidth, tI.insn.getWidth, tI.wdata.getWidth, tI.cause.getWidth, tI.tval.getWidth)

  // note: the wdata is not 0 here since we can't deal with 0 width wires
  def apply(tI: TracedInstruction): TracedInstructionWidths =
    TracedInstructionWidths(tI.iaddr.getWidth, tI.insn.getWidth, 1, tI.cause.getWidth, tI.tval.getWidth)
}

class ExtendedTracedInstruction(implicit p: Parameters) extends TracedInstruction {
  val wdata = UInt(xLen.W)
}

object ExtendedTracedInstruction {
  def apply(tI: TracedInstruction): ExtendedTracedInstruction = {
    val temp = Wire(new ExtendedTracedInstruction()(tI.p))
    temp.clock := tI.clock
    temp.reset := tI.reset
    temp.valid := tI.valid
    temp.iaddr := tI.iaddr
    temp.insn := tI.insn
    temp.wdata := 0.U
    temp.priv := tI.priv
    temp.exception := tI.exception
    temp.interrupt := tI.interrupt
    temp.cause := tI.cause
    temp.tval := tI.tval

    temp
  }

  def fromVec(tis: Vec[TracedInstruction]): Vec[ExtendedTracedInstruction] = {
    val tempVec = tis.map(insn => Wire(new ExtendedTracedInstruction()(insn.p)))

    tempVec.zip(tis).foreach({ case (ext, non_ext) =>
      ext.clock     := non_ext.clock
      ext.reset     := non_ext.reset
      ext.valid     := non_ext.valid
      ext.iaddr     := non_ext.iaddr
      ext.insn      := non_ext.insn
      ext.wdata     := 0.U
      ext.priv      := non_ext.priv
      ext.exception := non_ext.exception
      ext.interrupt := non_ext.interrupt
      ext.cause     := non_ext.cause
      ext.tval      := non_ext.tval
    })
    VecInit(tempVec)
  }
}

// Hack: In a457f658a, RC added the Clocked trait to TracedInstruction, which breaks midas
// I/O token handling. The non-Clock fields of this Bundle should be factored out in rocket chip.
// For now, we create second Bundle with Clock (of type Clock) and Reset removed
class DeclockedTracedInstruction(val widths: TracedInstructionWidths) extends Bundle {
  val valid = Bool()
  val iaddr = UInt(widths.iaddr.W)
  val insn = UInt(widths.insn.W)
  val wdata = UInt(widths.wdata.W)
  val priv = UInt(3.W)
  val exception = Bool()
  val interrupt = Bool()
  val cause = UInt(widths.cause.W)
  val tval = UInt(widths.tval.W)
}

object DeclockedTracedInstruction {
  def apply(tI: ExtendedTracedInstruction): DeclockedTracedInstruction =
    new DeclockedTracedInstruction(TracedInstructionWidths(tI))

  def apply(tI: TracedInstruction): DeclockedTracedInstruction =
    new DeclockedTracedInstruction(TracedInstructionWidths(tI))

  // Generates a hardware Vec of declockedInsns
  def fromExtVec(clockedVec: Vec[ExtendedTracedInstruction]): Vec[DeclockedTracedInstruction] = {
    val declockedVec = clockedVec.map(insn => Wire(DeclockedTracedInstruction(insn.cloneType)))
    declockedVec.zip(clockedVec).foreach({ case (declocked, clocked) =>
      declocked.valid := clocked.valid
      declocked.iaddr := clocked.iaddr
      declocked.insn := clocked.insn
      declocked.wdata := clocked.wdata
      declocked.priv := clocked.priv
      declocked.exception := clocked.exception
      declocked.interrupt := clocked.interrupt
      declocked.cause := clocked.cause
      declocked.tval := clocked.tval
    })
    VecInit(declockedVec)
  }

  def fromVec(clockedVec: Vec[TracedInstruction]): Vec[DeclockedTracedInstruction] = {
    val extInstVec = ExtendedTracedInstruction.fromVec(clockedVec)
    DeclockedTracedInstruction.fromExtVec(extInstVec)
  }

  // Generates a Chisel type from that returned by a Diplomatic node's in() or .out() methods
  def fromExtNode(ports: Seq[(Vec[ExtendedTracedInstruction], Any)]): Seq[Vec[DeclockedTracedInstruction]] = ports.map({
    case (bundle, _) => Vec(bundle.length, DeclockedTracedInstruction(bundle.head.cloneType))
  })

  // Generates a Chisel type from that returned by a Diplomatic node's in() or .out() methods
  def fromNode(ports: Seq[(Vec[TracedInstruction], Any)]): Seq[Vec[DeclockedTracedInstruction]] = ports.map({
    case (bundle, _) => Vec(bundle.length, DeclockedTracedInstruction(bundle.head.cloneType))
  })
}


// The IO matched on by the TracerV bridge: a wrapper around a heterogenous
// bag of vectors. Each entry is Vec of committed instructions
class TraceOutputTop(private val traceProto: Seq[Vec[DeclockedTracedInstruction]]) extends Bundle {
  val traces = Output(HeterogeneousBag(traceProto.map(_.cloneType)))
  def getProto() = traceProto
  def getWidths(): Seq[TracedInstructionWidths] = traceProto.map(_.head.widths)
  def getVecSizes(): Seq[Int] = traceProto.map(_.size)
}

object TraceOutputTop {
  def apply(widths: Seq[TracedInstructionWidths], vecSizes: Seq[Int]): TraceOutputTop = {
    new TraceOutputTop(vecSizes.zip(widths).map({ case (size, w) =>
      Vec(size, new DeclockedTracedInstruction(w))
    }))
  }
}

