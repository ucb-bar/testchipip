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
// TSI serial stream bundle (chip-top facing)
//   out: chip → MAC (Decoupled)
//   in:  MAC  → chip (Flipped(Decoupled))
// ============================================================================

class UDPTSISerialIO(val serialWidth: Int) extends Bundle {
  val out = Decoupled(UInt(serialWidth.W))
  val in  = Flipped(Decoupled(UInt(serialWidth.W)))
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
  val io = IO(new Bundle {
    val clk       = Input(Clock())
    val rst       = Input(Bool())
    val gtx_clk   = Input(Clock())
    val gtx_clk90 = Input(Clock())
    val gtx_rst   = Input(Bool())
    val clk_200   = Input(Clock())

    val rgmii_txd    = Output(UInt(4.W))
    val rgmii_tx_ctl = Output(Bool())
    val rgmii_txc    = Output(Clock())
    val rgmii_rxd    = Input(UInt(4.W))
    val rgmii_rx_ctl = Input(Bool())
    val rgmii_rxc    = Input(Clock())

    val phy_reset_n  = Output(Bool())
    val phy_link_up  = Output(Bool())

    val serial_out_bits  = Output(UInt(params.serialWidth.W))
    val serial_out_valid = Output(Bool())
    val serial_out_ready = Input(Bool())
    val serial_in_bits   = Input(UInt(params.serialWidth.W))
    val serial_in_valid  = Input(Bool())
    val serial_in_ready  = Output(Bool())

    val select_invert = Output(Bool())
  })
}

// ============================================================================
// Chipyard integration trait
//
// Only instantiates TSIToTileLink inside the chip.  The MAC (udp_tsi_top) is
// instantiated once in the harness via UDPTSIStreamMuxShim so that a single
// set of RGMII pads is shared across multiple chips.
//
// udp_tsi returns (serialIO, params) where serialIO is the internal signal
// that WithUDPTSIPunchthrough will connect to a chip-top IO port.
// ============================================================================

trait CanHavePeripheryUDPTSI { this: BaseSubsystem =>
  val udp_tsi = p(UDPTSIKey).map { params =>
    val fbus = locateTLBusWrapper(FBUS)

    val udpTsiDomain = LazyModule(new ClockSinkDomain(name = Some("udpTsi")))
    udpTsiDomain.clockNode := fbus.fixedClockNode

    val tsi2tl = udpTsiDomain { LazyModule(new TSIToTileLink) }
    fbus.coupleFrom("udp_tsi") { _ := TLBuffer() := tsi2tl.node }

    // Expose TSI serial streams out of the clock domain and then to chip top.
    val serial_inner = udpTsiDomain { InModuleBody {
      val serial = IO(new UDPTSISerialIO(params.serialWidth)).suggestName("udp_tsi_serial")
      serial.out.bits  := tsi2tl.module.io.tsi.out.bits
      serial.out.valid := tsi2tl.module.io.tsi.out.valid
      tsi2tl.module.io.tsi.out.ready := serial.out.ready
      tsi2tl.module.io.tsi.in.bits  := serial.in.bits
      tsi2tl.module.io.tsi.in.valid := serial.in.valid
      serial.in.ready := tsi2tl.module.io.tsi.in.ready
      serial
    }}

    val serial_outer = InModuleBody {
      val serial = IO(new UDPTSISerialIO(params.serialWidth)).suggestName("udp_tsi_serial")
      serial <> serial_inner
      serial
    }

    (serial_outer, params)
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
