// UDPToTSI.scala
//
// Chipyard integration for the UDP-TSI bridge using
// Alex Forencich's verilog-ethernet (open source, MIT license).
//
// Replaces CanHavePeripheryUARTTSI.
//
// Place this file in:
//   generators/testchipip/src/main/scala/testchipip/tsi/UDPToTSI.scala
//   (this Scala trait must live under generators/ for Chipyard to find it)
//
// Verilog sources live under fpga/:
//   fpga/udp_tsi_top.v
//   fpga/udp_payload_to_tsi_serial.v
//   fpga/verilog-ethernet/rtl/   (git submodule)
//
// Add the Verilog files to your Vivado project or FPGA build flow.
// They are NOT added via HasBlackBoxResource since they live outside
// the generators/ tree — instead, add them in your FPGA Makefile or
// Vivado TCL script (see README.md for the full file list).

package testchipip.tsi

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam, ExtModule}
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.prci.ClockSinkDomain


// ============================================================================
// Parameters
// ============================================================================

case class UDPTSIParams(
  fpgaMac:     Long = 0x000A35000102L,
  fpgaIp:      Long = (192L << 24) | (168L << 16) | (1L << 8) | 10L,
  fpgaGateway: Long = (192L << 24) | (168L << 16) | (1L << 8) | 1L,
  subnetMask:  Long = (255L << 24) | (255L << 16) | (255L << 8) | 0L,
  udpPort:     Int  = 7000,
  serialWidth: Int  = 32
)

case object UDPTSIKey extends Field[Option[UDPTSIParams]](None)

// ============================================================================
// RGMII port bundle
// ============================================================================

class RGMIIPort extends Bundle {
  val txd     = Output(UInt(4.W))
  val tx_ctl  = Output(Bool())
  val txc     = Output(Clock())
  val rxd     = Input(UInt(4.W))
  val rx_ctl  = Input(Bool())
  val rxc     = Input(Clock())
}

// ============================================================================
// BlackBox wrapping udp_tsi_top.v
// ============================================================================

class udp_tsi_top(params: UDPTSIParams) extends BlackBox(Map(
  "FPGA_MAC"     -> IntParam(params.fpgaMac),
  "FPGA_IP"      -> IntParam(params.fpgaIp),
  "FPGA_GATEWAY" -> IntParam(params.fpgaGateway),
  "SUBNET_MASK"  -> IntParam(params.subnetMask),
  "UDP_PORT"     -> IntParam(params.udpPort),
  "SERIAL_WIDTH" -> IntParam(params.serialWidth)
)) {
  // Verilog sources are provided by the FPGA build filelist (fpga/Makefile),
  // not via addPath/addResource.

  val io = IO(new Bundle {
    // Clocks
    val clk       = Input(Clock())
    val rst       = Input(Bool())
    val gtx_clk   = Input(Clock())
    val gtx_clk90 = Input(Clock())
    val gtx_rst   = Input(Bool())

    // RGMII
    val rgmii_txd    = Output(UInt(4.W))
    val rgmii_tx_ctl = Output(Bool())
    val rgmii_txc    = Output(Clock())
    val rgmii_rxd    = Input(UInt(4.W))
    val rgmii_rx_ctl = Input(Bool())
    val rgmii_rxc    = Input(Clock())

    // PHY
    val phy_reset_n  = Output(Bool())
    val phy_link_up  = Output(Bool())

    // TSI serial
    val serial_out_bits  = Output(UInt(params.serialWidth.W))
    val serial_out_valid = Output(Bool())
    val serial_out_ready = Input(Bool())
    val serial_in_bits   = Input(UInt(params.serialWidth.W))
    val serial_in_valid  = Input(Bool())
    val serial_in_ready  = Output(Bool())
  })

}

// ============================================================================
// Chipyard integration trait
// ============================================================================

trait CanHavePeripheryUDPTSI { this: BaseSubsystem =>
  val udp_tsi = p(UDPTSIKey).map { params =>
    val fbus = locateTLBusWrapper(FBUS)

    val udpTsiDomain = LazyModule(new ClockSinkDomain(name = Some("udp_tsi")))
    udpTsiDomain.clockNode := fbus.fixedClockNode
    
    val tsi2tl = udpTsiDomain { LazyModule(new TSIToTileLink) }
    fbus.coupleFrom("udp_tsi") { _ := TLBuffer() := tsi2tl.node }
    

    val udp_bus_io = udpTsiDomain { InModuleBody {
      // Expose RGMII and clock ports at chip top
      val rgmii     = IO(new RGMIIPort)
      val gtx_clk   = IO(Input(Clock()))
      val gtx_clk90 = IO(Input(Clock()))
      val phy_resetn = IO(Output(Bool()))
      val phy_link   = IO(Output(Bool()))

      val bridge = Module(new udp_tsi_top(params))

      // Clock connections
      bridge.io.clk       := tsi2tl.module.clock
      bridge.io.rst       := tsi2tl.module.reset.asBool
      bridge.io.gtx_clk   := gtx_clk
      bridge.io.gtx_clk90 := gtx_clk90
      bridge.io.gtx_rst   := tsi2tl.module.reset.asBool

      // RGMII
      rgmii.txd    := bridge.io.rgmii_txd
      rgmii.tx_ctl := bridge.io.rgmii_tx_ctl
      rgmii.txc    := bridge.io.rgmii_txc
      bridge.io.rgmii_rxd    := rgmii.rxd
      bridge.io.rgmii_rx_ctl := rgmii.rx_ctl
      bridge.io.rgmii_rxc    := rgmii.rxc

      // PHY
      phy_resetn := bridge.io.phy_reset_n
      phy_link   := bridge.io.phy_link_up

      // TSI
      tsi2tl.module.io.tsi.in.valid  := bridge.io.serial_out_valid
      tsi2tl.module.io.tsi.in.bits   := bridge.io.serial_out_bits
      bridge.io.serial_out_ready     := tsi2tl.module.io.tsi.in.ready

      bridge.io.serial_in_bits   := tsi2tl.module.io.tsi.out.bits
      bridge.io.serial_in_valid  := tsi2tl.module.io.tsi.out.valid
      tsi2tl.module.io.tsi.out.ready := bridge.io.serial_in_ready

      (rgmii, gtx_clk, gtx_clk90, phy_resetn, phy_link)
    }}

    val udp_tsi_io = InModuleBody {
      val rgmii      = IO(new RGMIIPort)
      val gtx_clk    = IO(Input(Clock()))
      val gtx_clk90  = IO(Input(Clock()))
      val phy_resetn = IO(Output(Bool()))
      val phy_link   = IO(Output(Bool()))

      rgmii <> udp_bus_io._1
      udp_bus_io._2 := gtx_clk
      udp_bus_io._3 := gtx_clk90
      phy_resetn := udp_bus_io._4
      phy_link := udp_bus_io._5

      (rgmii, gtx_clk, gtx_clk90, phy_resetn, phy_link)
    }

    udp_tsi_io
  }
}

// ============================================================================
// Config Fragments
// ============================================================================

class WithUDPTSI(
  fpgaMac:     Long = 0x000A35000102L,
  fpgaIp:      Long = (192L << 24) | (168L << 16) | (1L << 8) | 10L,
  fpgaGateway: Long = (192L << 24) | (168L << 16) | (1L << 8) | 1L,
  subnetMask:  Long = (255L << 24) | (255L << 16) | (255L << 8) | 0L,
  udpPort:     Int  = 7000,
  serialWidth: Int  = 32
) extends Config((site, here, up) => {
  case UDPTSIKey => Some(UDPTSIParams(
    fpgaMac     = fpgaMac,
    fpgaIp      = fpgaIp,
    fpgaGateway = fpgaGateway,
    subnetMask  = subnetMask,
    udpPort     = udpPort,
    serialWidth = serialWidth
  ))
})

class WithNoUARTTSI extends Config((site, here, up) => {
  case UARTTSIClientKey => None
})
