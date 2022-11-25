package testchipip

import chipsalliance.rocketchip.config.Field
import chisel3._
import freechips.rocketchip.diplomacy.{BundleBridgeNexusNode, LazyModuleImp}
import freechips.rocketchip.rocket.TraceDoctor
import freechips.rocketchip.subsystem.HasTiles
import freechips.rocketchip.util.HeterogeneousBag


// A per-tile interface that includes the tile's clock and reset
class TileTraceDoctorIO(val traceWidth: Int) extends Bundle {
  val clock: Clock = Clock()
  val reset: Bool = Bool()
  val data = new TraceDoctor(traceWidth)
}

// The IO matched on by the TraceDoctor bridge: a wrapper around a heterogenous
// bag of TileTraceDoctorIO. Each entry is trace associated with a single tile
class TraceDoctorOutputTop(val traceWidths: Seq[Int]) extends Bundle {
  val tracedoctors: HeterogeneousBag[TileTraceDoctorIO] = Output(HeterogeneousBag(traceWidths.map(w => new TileTraceDoctorIO(w))))
}

object TraceDoctorOutputTop {
  def apply(proto: Seq[TraceDoctor]): TraceDoctorOutputTop =
    new TraceDoctorOutputTop(proto.map(t => t.traceWidth))
}

// Use this trait:
trait CanHaveTraceDoctorIO { this: HasTiles =>
  val module: CanHaveTraceDoctorIOModuleImp
  // Bind all the trace nodes to a BB; we'll use this to generate the IO in the imp
  val traceDoctorNexus = BundleBridgeNexusNode[TraceDoctor]()
  tiles.foreach { traceDoctorNexus := _.traceDoctorNode }
}
case class TraceDoctorPortParams(print: Boolean = false)
object TraceDoctorPortKey extends Field[Option[TraceDoctorPortParams]](None)

trait CanHaveTraceDoctorIOModuleImp extends LazyModuleImp {
  val outer: CanHaveTraceDoctorIO with HasTiles

  val traceDoctorIO = p(TraceDoctorPortKey) map ( traceParams => {
    val traceDoctorSeq = (outer.traceDoctorNexus.in.map(_._1))
    val tio = IO(Output(TraceDoctorOutputTop(traceDoctorSeq)))

    (tio.tracedoctors zip (outer.tile_prci_domains zip traceDoctorSeq)).foreach { case (port, (prci, tracedoc)) =>
      port.clock := prci.module.clock
      port.reset := prci.module.reset.asBool
      port.data := tracedoc
    }

    if (traceParams.print) {
      for ((trace, idx) <- tio.tracedoctors.zipWithIndex ) {
        withClockAndReset(trace.clock, trace.reset) {
          when (trace.data.valid) {
            printf(s"TraceDoctor $idx: %x\n", trace.data.bits.asUInt)
          }
        }
      }
    }
    tio
  })
}



