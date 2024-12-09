package testchipip.tsi

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam}
import chisel3.experimental.dataview._

import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import testchipip.serdes._


case class SerialTSIParams(
  inner: SerialTLParams
)

case object SerialTSIKey extends Field[Option[SerialTSIParams]](None)

class SerialTSIIO() extends Bundle {
  val tsi = new TSIIO
}

trait CanHavePeripherySerialTSI { this: BaseSubsystem =>
  val numTLChannels = 5

  val serial_tsi = p(SerialTSIKey).map { params =>
    val name = s"serial_tsi"
    lazy val client_bus = params.inner.client.map(c => locateTLBusWrapper(c.masterWhere))
    val clientPortParams = params.inner.client.map { c => TLMasterPortParameters.v1(
      clients = Seq.tabulate(1 << c.cacheIdBits){ i => TLMasterParameters.v1(
        name = s"serial_tl_${i}",
        sourceId = IdRange(i << (c.totalIdBits - c.cacheIdBits), (i + 1) << (c.totalIdBits - c.cacheIdBits)),
        supportsProbe = if (c.supportsProbe) TransferSizes(client_bus.get.blockBytes, client_bus.get.blockBytes) else TransferSizes.none
      )}
    )}

    val serial_tl_domain = LazyModule(new ClockSinkDomain(name=Some(s"SerialTSI")))
    serial_tl_domain.clockNode := client_bus.get.fixedClockNode

    if (client_bus.isDefined) require(client_bus.get.dtsFrequency.isDefined,
      s"Client bus ${client_bus.get.busName} must provide a frequency")

    val serdesser = serial_tl_domain { LazyModule(new TLSerdesser(
      flitWidth = params.inner.phyParams.flitWidth,
      clientPortParams = clientPortParams,
      managerPortParams = None,
      bundleParams = params.inner.bundleParams,
      nameSuffix = Some(name)
    )) }

    serdesser.clientNode.foreach { clientNode =>
      client_bus.get.coupleFrom(s"port_named_${name}_in") { _ := TLBuffer() := clientNode }
    }


    // If we provide a clock, generate a clock domain for the outgoing clock
    val serial_tl_clock_freqMHz = params.inner.phyParams match {
      case params: InternalSyncSerialPhyParams => Some(params.freqMHz)
      case params: ExternalSyncSerialPhyParams => None
      case params: SourceSyncSerialPhyParams => Some(params.freqMHz)
    }
    val serial_tl_clock_node = serial_tl_clock_freqMHz.map { f =>
      serial_tl_domain { ClockSinkNode(Seq(ClockSinkParameters(take=Some(ClockParameters(f))))) }
    }
    serial_tl_clock_node.foreach(_ := ClockGroup()(p, ValName(s"${name}_clock")) := allClockGroupsNode)


    val tsi_inner_rst = serial_tl_domain { InModuleBody {
      val tsi_inner = IO(new TSIIO)
      val rst = IO(Bool())

      val ram = Module(LazyModule(new SerialRAM(serdesser, params.inner)(serdesser.p)).module)
      val outer_clock = serial_tl_clock_node.get.in.head._1.clock
      val phy = Module(new DecoupledSerialPhy(numTLChannels, params.inner.phyParams))
      phy.io.outer_clock := outer_clock
      phy.io.outer_reset := ResetCatchAndSync(outer_clock, serdesser.module.reset.asBool)
      phy.io.inner_clock := serdesser.module.clock
      phy.io.inner_reset := serdesser.module.reset
      phy.io.outer_ser.in <> ram.io.ser.out
      ram.io.ser.in <> phy.io.outer_ser.out

      phy.io.inner_ser <> serdesser.module.io.ser

      tsi_inner <> ram.io.tsi.get
      rst := phy.io.outer_reset
      (tsi_inner, rst)
    }}
    val tsi_outer = InModuleBody {
      val tsi_inner = tsi_inner_rst._1
      val rst = tsi_inner_rst._2

      val tsi_outer = IO(new TSIIO)
      tsi_outer.out <> tsi_inner.out
      tsi_inner.in.valid := tsi_outer.in.valid && !rst
      tsi_outer.in.ready := tsi_inner.in.ready && !rst
      tsi_inner.in.bits := tsi_outer.in.bits
      tsi_outer
    }
    tsi_outer
  }
}
