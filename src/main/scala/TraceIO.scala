package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.subsystem.{BaseSubsystem, HasTiles}
import freechips.rocketchip.config.{Field, Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, AddressSet, LazyModuleImpLike}
import freechips.rocketchip.tilelink.{TLRAM}
import freechips.rocketchip.rocket.{TracedInstruction}
import freechips.rocketchip.util._
import freechips.rocketchip.tile.{BaseTile}
import freechips.rocketchip.diplomacy.{BundleBridgeSource, BundleBroadcast, BundleBridgeNexusNode}

//***************************************************************************
// Trace Instruction Utilities:
// used to connect the TracerV or Dromajo bridges (in FireSim and normal sim)
//***************************************************************************

case class TracedInstructionWidths(iaddr: Int, insn: Int, wdata: Option[Int], cause: Int, tval: Int)

object TracedInstructionWidths {
  def apply(tI: TracedInstruction): TracedInstructionWidths = {
    val wdataWidth = tI.wdata.map { w => w.getWidth }
    TracedInstructionWidths(tI.iaddr.getWidth, tI.insn.getWidth, wdataWidth, tI.cause.getWidth, tI.tval.getWidth)
  }
}

// Hack: In a457f658a, RC added the Clocked trait to TracedInstruction, which breaks midas
// I/O token handling. The non-Clock fields of this Bundle should be factored out in rocket chip.
// For now, we create second Bundle with Clock (of type Clock) and Reset removed
//
// Follow up: clock and reset have since been removed removed (in 7b4efd4).
// However, the class remains difficult to instantiate outside of a
// Parameters context that looks a lot like rocket. Thus, it remains useful to
// keep these classes around though they should probably be renamed. Better
// yet, TracedInstruction's dependency on Parameters should be removed upstream.
class DeclockedTracedInstruction(val widths: TracedInstructionWidths) extends Bundle {
  val valid = Bool()
  val iaddr = UInt(widths.iaddr.W)
  val insn = UInt(widths.insn.W)
  val wdata = widths.wdata.map { w => UInt(w.W) }
  val priv = UInt(3.W)
  val exception = Bool()
  val interrupt = Bool()
  val cause = UInt(widths.cause.W)
  val tval = UInt(widths.tval.W)
}

object DeclockedTracedInstruction {
  def apply(tI: TracedInstruction): DeclockedTracedInstruction = {
    val dtI = Wire(new DeclockedTracedInstruction(TracedInstructionWidths(tI)))
    dtI.valid := tI.valid
    dtI.iaddr := tI.iaddr
    dtI.insn := tI.insn
    dtI.wdata.zip(tI.wdata).map { case (dc, c) => dc := c }
    dtI.priv := tI.priv
    dtI.exception := tI.exception
    dtI.interrupt := tI.interrupt
    dtI.cause := tI.cause
    dtI.tval := tI.tval
    dtI
  }

  def fromVec(clockedVec: Vec[TracedInstruction]): Vec[DeclockedTracedInstruction] = {
    VecInit(clockedVec.map(apply(_)))
  }

  // Generates a Chisel type from that returned by a Diplomatic node's in() or .out() methods
  def fromNode(ports: Seq[(Vec[TracedInstruction], Any)]): Seq[Vec[DeclockedTracedInstruction]] = ports.map({
    case (bundle, _) => Vec(bundle.length, DeclockedTracedInstruction(bundle.head.cloneType))
  })
}

// A per-tile interface that includes the tile's clock and reset
class TileTraceIO(val insnWidths: TracedInstructionWidths, val numInsns: Int) extends Bundle {
  val clock = Clock()
  val reset = Bool()
  val insns = Vec(numInsns, new DeclockedTracedInstruction(insnWidths))
}

// The IO matched on by the TracerV bridge: a wrapper around a heterogenous
// bag of vectors. Each entry is trace associated with a single tile (vector of committed instructions + clock + reset)
class TraceOutputTop(val widths: Seq[TracedInstructionWidths], val vecSizes: Seq[Int]) extends Bundle {
  val traces = Output(HeterogeneousBag(widths.zip(vecSizes).map({ case (w, n) => new TileTraceIO(w,n) })))
}

object TraceOutputTop {
  def apply(proto: Seq[Vec[TracedInstruction]]): TraceOutputTop =
    new TraceOutputTop(proto.map(t => TracedInstructionWidths(t.head)), proto.map(_.size))
}

//**********************************************
// Trace IO Key/Traits:
// Used to enable/add the tport on the top level
//**********************************************

case class TracePortParams(
  print: Boolean = false
)

object TracePortKey extends Field[Option[TracePortParams]](None)

trait CanHaveTraceIO { this: HasTiles =>
  val module: CanHaveTraceIOModuleImp

  // Bind all the trace nodes to a BB; we'll use this to generate the IO in the imp
  val traceNexus = BundleBridgeNexusNode[Vec[TracedInstruction]]()
  val tileTraceNodes = tiles.map { _.traceNode }

  // Convert all instructions to extended type
  tileTraceNodes.foreach { traceNexus := _ }
}

trait CanHaveTraceIOModuleImp { this: LazyModuleImpLike =>
  val outer: CanHaveTraceIO with HasTiles
  implicit val p: Parameters

  val traceIO = p(TracePortKey) map ( traceParams => {
    val traceSeqVec = outer.traceNexus.in.map(_._1)
    val tio = IO(Output(TraceOutputTop(traceSeqVec)))

    val tileInsts = ((outer.traceNexus.in) .map { case (tileTrace, _) => DeclockedTracedInstruction.fromVec(tileTrace) })

    // Since clock & reset are not included with the traced instruction, plumb that out manually
    (tio.traces zip (outer.tile_prci_domains zip tileInsts)).foreach { case (port, (prci, insts)) =>
      port.clock := prci.module.clock
      port.reset := prci.module.reset.asBool
      port.insns := insts
    }


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
