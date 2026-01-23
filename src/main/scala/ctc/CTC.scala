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
  phyParams: Option[SerialPhyParams] = Some(CreditedSourceSyncSerialPhyParams(phitWidth = CTC.OUTER_WIDTH, flitWidth = CTC.INNER_WIDTH, freqMHz = 100, flitBufferSz = 16)) // Set to None to disable PHY
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

      params.phyParams.map { pP => 
        assert(pP.phitWidth == CTC.OUTER_WIDTH)
        assert(pP.flitWidth == CTC.INNER_WIDTH)
      }

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
      val ctc_clock_freqMHz = params.phyParams match {
        case Some(param: DecoupledInternalSyncSerialPhyParams) => param.freqMHz
        case Some(param: DecoupledExternalSyncSerialPhyParams) => 100
        case Some(param: CreditedSourceSyncSerialPhyParams) => param.freqMHz
        case None => 100
      }
      val ctc_clock_node = ctc_domain { ClockSinkNode(Seq(ClockSinkParameters(take=Some(ClockParameters(ctc_clock_freqMHz))))) }
      ctc_clock_node := ClockGroup()(p, ValName(s"${ctc_name}_clock")) := allClockGroupsNode

      val ctc_outer_io = ctc_domain { InModuleBody {
        val outer_io = params.phyParams.map(pP => IO(pP.genIO).suggestName(ctc_name)).getOrElse(IO(new CTCBridgeIO).suggestName(ctc_name))
        
        outer_io match {
          case io: CreditedSourceSyncPhitIO => {
            // 3 clock domains -
            // - ctc2tl's "Inner clock": synchronizes signals going to the digital logic
            // - outgoing clock: synchronizes signals going out
            // - incoming clock: synchronizes signals coming in
            val outgoing_clock = ctc_clock_node.in.head._1.clock
            val outgoing_reset = ResetCatchAndSync(outgoing_clock, ctc2tl.module.reset.asBool)
            val incoming_clock = io.clock_in
            val incoming_reset = ResetCatchAndSync(incoming_clock, io.reset_in.asBool)
            io.clock_out := outgoing_clock
            io.reset_out := outgoing_reset.asAsyncReset
            val phy = Module(new CreditedSerialPhy(2, params.phyParams.get))
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

            phy.io.outer_ser <> io.viewAsSupertype(new ValidPhitIO(params.phyParams.get.phitWidth))
          }
          case io: DecoupledInternalSyncPhitIO => {
            val outgoing_clock = ctc_clock_node.in.head._1.clock
            io.clock_out := outgoing_clock
            val phy = Module(new DecoupledSerialPhy(2, params.phyParams.get))
            phy.io.outer_clock := outgoing_clock
            phy.io.outer_reset := ResetCatchAndSync(outgoing_clock, ctc2tl.module.reset.asBool)
            phy.io.inner_clock := ctc2tl.module.clock
            phy.io.inner_reset := ctc2tl.module.reset
            phy.io.inner_ser(0).in <> ctc2tl.module.io.flit.in
            phy.io.inner_ser(0).out <> tl2ctc.module.io.flit.out
            phy.io.inner_ser(1).in <> tl2ctc.module.io.flit.in
            phy.io.inner_ser(1).out <> ctc2tl.module.io.flit.out
            phy.io.outer_ser <> io.viewAsSupertype(new DecoupledPhitIO(params.phyParams.get.phitWidth))
          }
          case io: DecoupledExternalSyncPhitIO => {
            val outgoing_clock = io.clock_in
            val outgoing_reset = ResetCatchAndSync(outgoing_clock, ctc2tl.module.reset.asBool)
            val phy = Module(new DecoupledSerialPhy(2, params.phyParams.get))
            phy.io.outer_clock := outgoing_clock
            phy.io.outer_reset := ResetCatchAndSync(outgoing_clock, ctc2tl.module.reset.asBool)
            phy.io.inner_clock := ctc2tl.module.clock
            phy.io.inner_reset := ctc2tl.module.reset
            phy.io.inner_ser(0).in <> ctc2tl.module.io.flit.in
            phy.io.inner_ser(0).out <> tl2ctc.module.io.flit.out
            phy.io.inner_ser(1).in <> tl2ctc.module.io.flit.in
            phy.io.inner_ser(1).out <> ctc2tl.module.io.flit.out
            phy.io.outer_ser <> io.viewAsSupertype(new DecoupledPhitIO(params.phyParams.get.phitWidth))
          }
          case io: CTCBridgeIO => {
            io.manager_flit <> tl2ctc.module.io.flit
            io.client_flit <> ctc2tl.module.io.flit
          }
        }
        outer_io
      }}

      val outer_io = InModuleBody {
        val outer_io = params.phyParams.map(pP => IO(pP.genIO).suggestName(ctc_name)).getOrElse(IO(new CTCBridgeIO).suggestName(ctc_name))
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

class WithCTCPhyParams(phyParams: SerialPhyParams = CreditedSourceSyncSerialPhyParams(phitWidth = CTC.OUTER_WIDTH, flitWidth = CTC.INNER_WIDTH, freqMHz = 100, flitBufferSz = 16)) extends Config((site, here, up) => {
  case CTCKey => up(CTCKey).map(p => p.copy(phyParams = Some(phyParams)))
})

class WithCTCDecoupledPhy extends Config((site, here, up) => {
  case CTCKey => up(CTCKey).map(p => p.copy(phyParams = Some(DecoupledInternalSyncSerialPhyParams(phitWidth = CTC.OUTER_WIDTH, flitWidth = CTC.INNER_WIDTH, freqMHz = 100, flitBufferSz = 16))))
})

class WithNoCTCPhy extends Config((site, here, up) => {
  case CTCKey => up(CTCKey).map(p => p.copy(phyParams = None))
})
