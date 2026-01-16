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
import testchipip.soc._

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
  translationParams: AddressTranslatorParams = OutwardAddressTranslatorParams(onchipAddr = 0x1000000000L, offchipAddr = 0x0L, size = ((1L << 32) - 1)),
  offchip: Seq[AddressSet] = Nil,
  managerBus: Option[TLBusWrapperLocation] = Some(SBUS),
  clientBus: Option[TLBusWrapperLocation] = Some(SBUS),
  phyFreqMHz: Int = 100,
  noPhy: Boolean = false
) {
  def offchipRange = translationParams match {
    case OutwardAddressTranslatorParams(_,_,_) => Seq(translationParams.offchipRange)
    case InwardAddressTranslatorParams(_,_,_) => {
      require(offchip != Nil)
      offchip
    }
  }
}

// For using CTC in a chiplet firesim config with no PHY
class CTCBridgeIO extends Bundle {
  val client_flit = new DecoupledFlitIO(CTC.INNER_WIDTH) // Driven by client/ctc2tl
  val manager_flit = new DecoupledFlitIO(CTC.INNER_WIDTH) // Driven by manager/tl2ctc
}

case object CTCKey extends Field[Seq[CTCParams]](Nil)

trait CanHavePeripheryCTC { this: BaseSubsystem =>
  private val portName = "ctc"

  val (ctc2tls, tl2ctcs, ctc_ios) = p(CTCKey).zipWithIndex.map {
    case (params, id) => {

      val ctc_name = s"ctc$id"

      val phyParams = CreditedSourceSyncSerialPhyParams(
        phitWidth = CTC.OUTER_WIDTH,
        flitWidth = CTC.INNER_WIDTH,
        freqMHz = params.phyFreqMHz,
        flitBufferSz = 16
      )

      lazy val slave_bus = locateTLBusWrapper(params.managerBus.get)
      lazy val master_bus = locateTLBusWrapper(params.clientBus.get)

      val ctc_domain = LazyModule(new ClockSinkDomain(name=Some(s"CTC$id")))
      ctc_domain.clockNode := slave_bus.fixedClockNode

      val translator = ctc_domain {
        LazyModule(AddressTranslator(params.translationParams)(p))
      }

      require(slave_bus.dtsFrequency.isDefined,
        s"Slave bus ${slave_bus.busName} must provide a frequency")
      require(master_bus.dtsFrequency.isDefined,
        s"Master bus ${master_bus.busName} must provide a frequency")
      require(slave_bus.dtsFrequency == master_bus.dtsFrequency,
        s"Mismatching slave freq ${slave_bus.dtsFrequency} != master freq ${master_bus.dtsFrequency}")

      // a TL master/client device
      val ctc2tl = ctc_domain { LazyModule(new CTCToTileLink(portId=id)(p)) }
      // a TL slave/manager device
      val tl2ctc = ctc_domain { LazyModule(new TileLinkToCTC(addrRegion=params.offchipRange)(p)) }

      params.translationParams match {
        // Translate outgoing requests
        case OutwardAddressTranslatorParams(_,_,_) => {
          slave_bus.coupleTo(portName) { translator(tl2ctc.node) := TLBuffer() := _ }
          master_bus.coupleFrom(portName) { _ := TLBuffer() := ctc2tl.node }
        }
        // Translate incoming requests
        case InwardAddressTranslatorParams(_,_,_) => {
          slave_bus.coupleTo(portName) { tl2ctc.node := TLBuffer() := _ }
          master_bus.coupleFrom(portName) { _ := TLBuffer() := translator(ctc2tl.node) }
        }
      }
      
      // If we provide a clock, generate a clock domain for the outgoing clock
      val serial_tl_clock_freqMHz = CreditedSourceSyncSerialPhyParams().freqMHz
      val serial_tl_clock_node = ctc_domain { ClockSinkNode(Seq(ClockSinkParameters(take=Some(ClockParameters(serial_tl_clock_freqMHz))))) }
      serial_tl_clock_node := ClockGroup()(p, ValName(s"${ctc_name}_clock")) := allClockGroupsNode

      val ctc_outer_io = ctc_domain { InModuleBody {
        if (params.noPhy) {
          val flit_io = IO(new CTCBridgeIO).suggestName(ctc_name)
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
        val outer_io = if (params.noPhy) IO(new CTCBridgeIO).suggestName(ctc_name) else IO(phyParams.genIO).suggestName(ctc_name)
        outer_io <> ctc_outer_io
        outer_io
      }

      (ctc2tl, tl2ctc, outer_io)
    }
  }.unzip3
}


class WithCTC(params: Seq[CTCParams] = Seq(CTCParams())) extends Config((site, here, up) => {
  case CTCKey => params
})
