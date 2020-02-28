package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.subsystem.{BaseSubsystem, HasTiles}
import freechips.rocketchip.config.{Field, Config}
import freechips.rocketchip.diplomacy.{LazyModule, AddressSet, LazyModuleImp, BundleBridgeNexus}
import freechips.rocketchip.tilelink.{TLRAM}
import freechips.rocketchip.rocket.TracedInstruction
import freechips.rocketchip.util._

case class TracePortParams(
  print: Boolean = false
)

object TracePortKey extends Field[Option[TracePortParams]](None)


case class TracedInstructionWidths(iaddr: Int, insn: Int, cause: Int, tval: Int)

object TracedInstructionWidths {
  def apply(tI: TracedInstruction): TracedInstructionWidths =
    TracedInstructionWidths(tI.iaddr.getWidth, tI.insn.getWidth, tI.cause.getWidth, tI.tval.getWidth)
}

// Hack: In a457f658a, RC added the Clocked trait to TracedInstruction, which breaks midas
// I/O token handling. The non-Clock fields of this Bundle should be factored out in rocket chip.
// For now, we create second Bundle with Clock (of type Clock) and Reset removed
class DeclockedTracedInstruction(val widths: TracedInstructionWidths) extends Bundle {
  val valid = Bool()
  val iaddr = UInt(widths.iaddr.W)
  val insn = UInt(widths.insn.W)
  val priv = UInt(width = 3.W)
  val exception = Bool()
  val interrupt = Bool()
  val cause = UInt(widths.cause.W)
  val tval = UInt(widths.tval.W)
}

object DeclockedTracedInstruction {
  def apply(tI: TracedInstruction): DeclockedTracedInstruction =
    new DeclockedTracedInstruction(TracedInstructionWidths(tI))

  // Generates a hardware Vec of declockedInsns
  def fromVec(clockedVec: Vec[TracedInstruction]): Vec[DeclockedTracedInstruction] = {
    val declockedVec = clockedVec.map(insn => Wire(DeclockedTracedInstruction(insn.cloneType)))
    declockedVec.zip(clockedVec).foreach({ case (declocked, clocked) =>
      declocked.valid := clocked.valid
      declocked.iaddr := clocked.iaddr
      declocked.insn := clocked.insn
      declocked.priv := clocked.priv
      declocked.exception := clocked.exception
      declocked.interrupt := clocked.interrupt
      declocked.cause := clocked.cause
      declocked.tval := clocked.tval
    })
    VecInit(declockedVec)
  }

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

trait CanHaveTraceIO { this: HasTiles =>
  val module: CanHaveTraceIOModuleImp

  // Bind all the trace nodes to a BB; we'll use this to generate the IO in the imp
  val traceNexus = BundleBridgeNexus[Vec[TracedInstruction]]
  val tileTraceNodes = tiles.map(tile => tile.traceNode)
  tileTraceNodes foreach { traceNexus := _ }
}

trait CanHaveTraceIOModuleImp extends LazyModuleImp {
  val outer: CanHaveTraceIO

  val traceIO = p(TracePortKey) map ( p => {
    val tio = IO(Output(new TraceOutputTop(DeclockedTracedInstruction.fromNode(outer.traceNexus.in))))
    (tio.traces zip outer.traceNexus.in).foreach({ case (port, (tileTrace, _)) =>
      port := DeclockedTracedInstruction.fromVec(tileTrace)
    })
    if (p.print) {
      val traceprint = Wire(UInt(512.W))
      traceprint := Cat(tio.traces.map(_.reverse.asUInt))
      printf("TRACEPORT: %x\n", traceprint)
    }
    tio
  })
}
