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
// Extended Trace Instruction Utilities:
// Used to map TracedInstructions to their Extended version and
// used to connect the TracerV or Dromajo bridges (in FireSim and normal sim)
//***************************************************************************

case class TracedInstructionWidths(iaddr: Int, insn: Int, wdata: Option[Int], cause: Int, tval: Int)

object TracedInstructionWidths {
  def apply(tI: ExtendedTracedInstruction): TracedInstructionWidths = {
    val wdataWidth = tI.wdata.map { w => w.getWidth }
    TracedInstructionWidths(tI.iaddr.getWidth, tI.insn.getWidth, wdataWidth, tI.cause.getWidth, tI.tval.getWidth)
  }

  def apply(tI: TracedInstruction): TracedInstructionWidths =
    TracedInstructionWidths(tI.iaddr.getWidth, tI.insn.getWidth, None, tI.cause.getWidth, tI.tval.getWidth)
}

class ExtendedTracedInstruction(val extended: Boolean = true)(implicit p: Parameters) extends TracedInstruction {
  val wdata = if (extended) Some(UInt(xLen.W)) else None
}

object ExtendedTracedInstruction {
  def apply(tI: TracedInstruction): ExtendedTracedInstruction = {
    val temp = Wire(new ExtendedTracedInstruction(extended=false)(tI.p))
    temp.valid := tI.valid
    temp.iaddr := tI.iaddr
    temp.insn := tI.insn
    temp.priv := tI.priv
    temp.exception := tI.exception
    temp.interrupt := tI.interrupt
    temp.cause := tI.cause
    temp.tval := tI.tval

    temp
  }

  def fromVec(tis: Vec[TracedInstruction]): Vec[ExtendedTracedInstruction] = {
    VecInit(tis.map(insn => ExtendedTracedInstruction(insn)))
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
      declocked.wdata.zip(clocked.wdata).map { case (dc, c) => dc := c }
      declocked.priv := clocked.priv
      declocked.exception := clocked.exception
      declocked.interrupt := clocked.interrupt
      declocked.cause := clocked.cause
      declocked.tval := clocked.tval
    })
    VecInit(declockedVec)
  }

  def fromVec(clockedVec: Vec[TracedInstruction]): Vec[DeclockedTracedInstruction] = {
    DeclockedTracedInstruction.fromExtVec(ExtendedTracedInstruction.fromVec(clockedVec))
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
  def apply(proto: Seq[Vec[ExtendedTracedInstruction]]): TraceOutputTop =
    new TraceOutputTop(proto.map(t => TracedInstructionWidths(t.head)), proto.map(_.size))
}

//*****************************************************************
// Allow BaseTiles to have an ExtendedTracedInstruction port/bundle
//*****************************************************************

trait WithExtendedTraceport { this: BaseTile =>
  // Extended Traceport
  val extTraceSourceNode = BundleBridgeSource(() => Vec(tileParams.core.retireWidth, new ExtendedTracedInstruction()))
  val extTraceNode = BundleBroadcast[Vec[ExtendedTracedInstruction]](Some("trace"))
  extTraceNode := extTraceSourceNode
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
  val tileTraceNodes = tiles.flatMap {
    case ext_tile: WithExtendedTraceport => None
    case tile => Some(tile)
  }.map { _.traceNode }

  val extTraceNexus = BundleBridgeNexusNode[Vec[ExtendedTracedInstruction]]()
  val extTileTraceNodes = tiles.flatMap {
    case ext_tile: WithExtendedTraceport => Some(ext_tile)
    case tile => None
  }.map { _.extTraceNode }

  // Convert all instructions to extended type
  tileTraceNodes.foreach { traceNexus := _ }
  extTileTraceNodes.foreach { extTraceNexus := _ }
}

trait CanHaveTraceIOModuleImp { this: LazyModuleImpLike =>
  val outer: CanHaveTraceIO with HasTiles
  implicit val p: Parameters

  val traceIO = p(TracePortKey) map ( traceParams => {
    val extTraceSeqVec = (outer.traceNexus.in.map(_._1)).map(ExtendedTracedInstruction.fromVec(_)) ++ outer.extTraceNexus.in.map(_._1)
    val tio = IO(Output(TraceOutputTop(extTraceSeqVec)))

    val tileInsts = ((outer.traceNexus.in) .map { case (tileTrace, _) => DeclockedTracedInstruction.fromVec(tileTrace) } ++
      (outer.extTraceNexus.in) .map { case (tileTrace, _) => DeclockedTracedInstruction.fromExtVec(tileTrace) })

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

