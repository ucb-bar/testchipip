package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.subsystem.{BaseSubsystem, HasTiles}
import org.chipsalliance.cde.config.{Field, Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, AddressSet, LazyModuleImpLike}
import freechips.rocketchip.tilelink.{TLRAM}
import freechips.rocketchip.rocket.{TracedInstruction}
import freechips.rocketchip.util._
import freechips.rocketchip.tile.{BaseTile, TraceBundle}
import freechips.rocketchip.diplomacy.{BundleBridgeSource, BundleBroadcast, BundleBridgeNexusNode}

//***************************************************************************
// Trace Instruction Utilities:
// used to connect the TracerV or Dromajo bridges (in FireSim and normal sim)
//***************************************************************************

// A per-tile interface that includes the tile's clock and reset
class TileTraceIO(coreTrace: TraceBundle) extends Bundle {
  val clock = Clock()
  val reset = Bool()
  val trace = coreTrace.cloneType
  def numInsns = trace.insns.size
}

// The IO matched on by the TracerV bridge: a wrapper around a heterogenous
// bag of vectors. Each entry is trace associated with a single tile (vector of committed instructions + clock + reset)
class TraceOutputTop(coreTraces: Seq[TraceBundle]) extends Bundle {
  val traces = Output(HeterogeneousBag(coreTraces.map(t => new TileTraceIO(t))))
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
  val traceNexus = BundleBridgeNexusNode[TraceBundle]()
  val tileTraceNodes = tiles.map { _.traceNode }

  // Convert all instructions to extended type
  tileTraceNodes.foreach { traceNexus := _ }
}

trait CanHaveTraceIOModuleImp { this: LazyModuleImpLike =>
  val outer: CanHaveTraceIO with HasTiles
  implicit val p: Parameters

  val traceIO = p(TracePortKey) map ( traceParams => {
    val traceSeqVec = outer.traceNexus.in.map(_._1)
    val tio = IO(Output(new TraceOutputTop(traceSeqVec)))

    val tileTraces = outer.traceNexus.in.map(_._1)

    // Since clock & reset are not included with the traced instruction, plumb that out manually
    (tio.traces zip (outer.tile_prci_domains zip tileTraces)).foreach { case (port, (prci, trace)) =>
      port.clock := prci.module.clock
      port.reset := prci.module.reset.asBool
      port.trace := trace
    }


    if (traceParams.print) {
      for ((trace, idx) <- tio.traces.zipWithIndex ) {
        withClockAndReset(trace.clock, trace.reset) {
          // The reverse is here to match the behavior the Cat used in the bridge
          printf(s"TRACEPORT ${idx}: %x\n", trace.trace.insns.reverse.asUInt.pad(512))
        }
      }
    }

    tio
  })
}
