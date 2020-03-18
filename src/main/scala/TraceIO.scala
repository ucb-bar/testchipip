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

// A per-tile interface that pulls out the clock and reset into an enclosing
// bundle so they aren't duplicated k-ways
class TileTraceIO(val insnWidths: TracedInstructionWidths, val numInsns: Int) extends Bundle {
  val clock = Clock()
  val reset = Bool()
  val insns = Vec(numInsns, new DeclockedTracedInstruction(insnWidths))
}

// The IO matched on by the TracerV bridge: a wrapper around a heterogenous
// bag of vectors. Each entry is Vec of committed instructions
class TraceOutputTop(val widths: Seq[TracedInstructionWidths], val vecSizes: Seq[Int]) extends Bundle {
  val traces = Output(HeterogeneousBag(widths.zip(vecSizes).map({ case (w, n) => new TileTraceIO(w,n) })))
}

object TraceOutputTop {
  def apply(proto: Seq[Vec[TracedInstruction]]): TraceOutputTop =
    new TraceOutputTop(proto.map(t => TracedInstructionWidths(t.head)), proto.map(_.size))
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

  val traceIO = p(TracePortKey) map ( traceParams => {
    val tio = IO(Output(TraceOutputTop(outer.traceNexus.in.map(_._1))))
    (tio.traces zip outer.traceNexus.in).foreach({ case (port, (tileTrace, _)) =>
      port.clock := tileTrace.head.clock
      port.reset := tileTrace.head.reset
      port.insns := DeclockedTracedInstruction.fromVec(tileTrace)
    })

    if (traceParams.print) {
      for ((trace, idx) <- tio.traces.zipWithIndex ) {
        withClockAndReset(trace.clock, trace.reset) {
          // The reverse is here to match the behavior the Cat used in the bridge
          printf(s"TRACEPORT ${idx}: %x\n", trace.insns.reverse.asUInt.pad(512))
        }
      }
    }
    tio
  })
}
