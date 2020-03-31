package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.subsystem.{BaseSubsystem, HasTiles}
import freechips.rocketchip.config.{Field, Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, AddressSet, LazyModuleImp, BundleBridgeNexus}
import freechips.rocketchip.tilelink.{TLRAM}
import freechips.rocketchip.rocket.{TracedInstruction}
import freechips.rocketchip.util._
import freechips.rocketchip.tile.{BaseTile}
import freechips.rocketchip.diplomacy.{BundleBridgeSource, BundleBroadcast, BundleBridgeNexus}

//***************************************************************************
// Extended Trace Instruction Utilities:
// Used to map TracedInstructions to their Extended version and
// used to connect the TracerV or Dromajo bridges (in FireSim and normal sim)
//***************************************************************************

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

// A per-tile interface that pulls out the clock and reset into an enclosing
// bundle so they aren't duplicated k-ways
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
  def apply(proto: Seq[Vec[TracedInstruction]], protoExt: Seq[Vec[ExtendedTracedInstruction]]): TraceOutputTop =
    new TraceOutputTop(
      proto.map(t => TracedInstructionWidths(t.head)) ++ protoExt.map(t => TracedInstructionWidths(t.head)),
      proto.map(_.size) ++ protoExt.map(_.size))
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
  val traceNexus = BundleBridgeNexus[Vec[TracedInstruction]]
  val tileTraceNodes = tiles.flatMap {
    case ext_tile: WithExtendedTraceport => None
    case tile => Some(tile)
  }.map { _.traceNode }

  val extTraceNexus = BundleBridgeNexus[Vec[ExtendedTracedInstruction]]
  val extTileTraceNodes = tiles.flatMap {
    case ext_tile: WithExtendedTraceport => Some(ext_tile)
    case tile => None
  }.map { _.extTraceNode }

  // Convert all instructions to extended type
  tileTraceNodes.foreach { traceNexus := _ }
  extTileTraceNodes.foreach { extTraceNexus := _ }
}

trait CanHaveTraceIOModuleImp extends LazyModuleImp {
  val outer: CanHaveTraceIO

  val traceIO = p(TracePortKey) map ( traceParams => {
    val tio = IO(Output(TraceOutputTop(outer.traceNexus.in.map(_._1), outer.extTraceNexus.in.map(_._1))))

    val declkedPorts = (outer.traceNexus.in).map {
      case (tileTrace, _) => (tileTrace.head.clock, tileTrace.head.reset, DeclockedTracedInstruction.fromVec(tileTrace))
    } ++ (outer.extTraceNexus.in).map {
      case (tileTrace, _) => (tileTrace.head.clock, tileTrace.head.reset, DeclockedTracedInstruction.fromExtVec(tileTrace))
    }

    (tio.traces zip declkedPorts).foreach({ case (port, (clk, rst, insts)) =>
      port.clock := clk
      port.reset := rst
      port.insns := insts
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

