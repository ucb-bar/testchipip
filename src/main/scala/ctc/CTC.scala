package testchipip.ctc

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._

import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.prci._
import freechips.rocketchip.util.ResetCatchAndSync
// import testchipip.soc.{SBUS}

import testchipip.serdes._


object CTC {
  val INNER_WIDTH = 32
  val INNER_WIDTH_BYTES = INNER_WIDTH / 8
  val OUTER_WIDTH = 4
}

object CTCCommand {
  val read_req = 0.U
  val write_req = 1.U
  val read_ack = 2.U
  val write_ack = 3.U
}

case class CTCParams(
  onchipAddr: BigInt = 0x100000000L, // addresses that get routed here from THIS chip
  offchipAddr: BigInt = 0x0, // addresses that this ctc device can access on the OTHER chip
  size: BigInt = ((1L << 10) - 1), // 1024 bytes
  managerBus: Option[TLBusWrapperLocation] = Some(SBUS),
  clientBus: Option[TLBusWrapperLocation] = Some(SBUS),
  phyFreqMHz: Int = 100,
  noPhy: Boolean = false
)

// For using CTC in a chiplet firesim config with no PHY
class CTCBridgeIO extends Bundle {
  val client_flit = new DecoupledFlitIO(CTC.INNER_WIDTH) // Driven by client/ctc2tl
  val manager_flit = new DecoupledFlitIO(CTC.INNER_WIDTH) // Driven by manager/tl2ctc
}

case object CTCKey extends Field[Option[CTCParams]](None)

trait CanHavePeripheryCTC { this: BaseSubsystem =>
  private val portName = "ctc"

  val ctc_name = s"ctc"
  val (ctc2tl, tl2ctc, ctc_io) = p(CTCKey) match {
    case Some(params) => {

      val phyParams = CreditedSourceSyncSerialPhyParams(
        phitWidth = CTC.OUTER_WIDTH,
        flitWidth = CTC.INNER_WIDTH,
        freqMHz = params.phyFreqMHz,
        flitBufferSz = 16
      )

      lazy val slave_bus = locateTLBusWrapper(params.managerBus.get)
      lazy val master_bus = locateTLBusWrapper(params.clientBus.get)

      val ctc_domain = LazyModule(new ClockSinkDomain(name=Some(s"CTC")))
      ctc_domain.clockNode := slave_bus.fixedClockNode

      val translator = ctc_domain {
        LazyModule(InwardAddressTranslator(AddressSet(params.offchipAddr, params.size), Some(params.onchipAddr))(p))
      }

      require(slave_bus.dtsFrequency.isDefined,
        s"Slave bus ${slave_bus.busName} must provide a frequency")
      require(master_bus.dtsFrequency.isDefined,
        s"Master bus ${master_bus.busName} must provide a frequency")
      require(slave_bus.dtsFrequency == master_bus.dtsFrequency,
        s"Mismatching slave freq ${slave_bus.dtsFrequency} != master freq ${master_bus.dtsFrequency}")


      // slave
      val ctc2tl = ctc_domain { LazyModule(new CTCToTileLink()(p)) }
      // master
      val tl2ctc = ctc_domain { LazyModule(new TileLinkToCTC(baseAddr=params.offchipAddr, size=params.size)(p)) }

      slave_bus.coupleTo(portName) { translator(tl2ctc.node) := TLBuffer() := _ }
      master_bus.coupleFrom(portName) { _ := TLBuffer() := ctc2tl.node }
      
      // If we provide a clock, generate a clock domain for the outgoing clock
      val serial_tl_clock_freqMHz = CreditedSourceSyncSerialPhyParams().freqMHz
      val serial_tl_clock_node = ctc_domain { ClockSinkNode(Seq(ClockSinkParameters(take=Some(ClockParameters(serial_tl_clock_freqMHz))))) }
      serial_tl_clock_node := ClockGroup()(p, ValName(s"${ctc_name}_clock")) := allClockGroupsNode

      val ctc_outer_io = ctc_domain { InModuleBody {
        if (params.noPhy) {
          val flit_io = IO(new CTCBridgeIO)
          flit_io.manager_flit <> tl2ctc.module.io.flit
          flit_io.client_flit <> ctc2tl.module.io.flit
          flit_io
        } else {
          val phit_io = IO(phyParams.genIO).suggestName(ctc_name)

          // 3 clock domains -
          // - ctc2tl's "Inner clock": synchronizes signals going to the digital logic
          // - outgoing clock: synchronizes signals going out
          // - incoming clock: synchronizes signals coming in
          val outgoing_clock = serial_tl_clock_node.in.head._1.clock
          val outgoing_reset = ResetCatchAndSync(outgoing_clock, ctc2tl.module.reset.asBool)
          val incoming_clock = phit_io.clock_in
          val incoming_reset = ResetCatchAndSync(incoming_clock, phit_io.reset_in.asBool)
          phit_io.clock_out := outgoing_clock
          phit_io.reset_out := outgoing_reset.asAsyncReset
          val phy = Module(new CreditedSerialPhy(2, phyParams))
          phy.io.incoming_clock := incoming_clock
          phy.io.incoming_reset := incoming_reset
          phy.io.outgoing_clock := outgoing_clock
          phy.io.outgoing_reset := outgoing_reset
          phy.io.inner_clock := ctc2tl.module.clock
          phy.io.inner_reset := ctc2tl.module.reset
          phy.io.inner_ser(0).in <> ctc2tl.module.io.flit.in
          phy.io.inner_ser(0).out <> tl2ctc.module.io.flit.out
          phy.io.inner_ser(1).in <> tl2ctc.module.io.flit.in
          phy.io.inner_ser(1).out <> ctc2tl.module.io.flit.out

          phy.io.outer_ser <> phit_io.viewAsSupertype(new ValidPhitIO(phyParams.phitWidth))
          phit_io
        }
      }}

      val outer_io = InModuleBody {
        val outer_io = if (params.noPhy) IO(new CTCBridgeIO) else IO(phyParams.genIO).suggestName(ctc_name)
        outer_io <> ctc_outer_io
        outer_io
      }

      // val inner_debug_io = ctc_domain { InModuleBody {
      //   val inner_debug_io = IO(new SerdesDebugIO).suggestName(s"${ctc_name}_debug")
      //   inner_debug_io := ctc2tl.module.io.debug
      //   inner_debug_io
      // }}
      // val outer_debug_io = InModuleBody {
      //   val outer_debug_io = IO(new SerdesDebugIO).suggestName(s"${ctc_name}_debug")
      //   outer_debug_io := inner_debug_io
      //   outer_debug_io
      // }

      (ctc2tl, tl2ctc, Some(outer_io))
    }
    case None => (None, None, None)
  }
}


class WithCTC(params: CTCParams = CTCParams()) extends Config((site, here, up) => {
  case CTCKey => Some(params)
})

case class InwardAddressTranslator(blockRange : AddressSet, replicationBase : Option[BigInt] = None)(implicit p: Parameters) extends LazyModule {
  val module_side = replicationBase.map { base =>
    val baseRegion   = AddressSet(0, base-1)
    val replicator   = LazyModule(new RegionReplicator(ReplicatedRegion(baseRegion, baseRegion.widen(base))))
    val prefixSource = BundleBridgeSource[UInt](() => UInt(1.W))
    replicator.prefix := prefixSource
    InModuleBody { prefixSource.bundle := 0.U(1.W) } // prefix is unused for TL uncached, so this is ok
    replicator.node
  }.getOrElse { TLTempNode() }

  // val bus_side = TLFilter(TLFilter.mSelectIntersect(blockRange))(p)
  val bus_side = TLFilter(TLFilter.mSubtract(blockRange))(p)

  // module_side := bus_side

  def apply(node : TLNode) : TLNode = {
    node := module_side := bus_side
  }

  lazy val module = new LazyModuleImp(this) {}
}