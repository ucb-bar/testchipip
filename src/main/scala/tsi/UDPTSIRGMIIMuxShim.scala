package testchipip.tsi

import chisel3._
import chisel3.util._

// Instantiates a single udp_tsi_top MAC and muxes two chips' TSI serial
// streams in front of it.  The select signal (driven by a board switch)
// chooses which chip communicates over Ethernet.  Muxing at the stream level
// keeps the ODDR primitives inside the single MAC connected directly to the
// output buffer, satisfying Vivado DRC REQP-1884.
class UDPTSIStreamMuxShim(params: UDPTSIParams) extends Module {
  val io = IO(new Bundle {
    val select = Input(Bool())

    val phyRgmii  = new RGMIIPort
    val phyResetN = Output(Bool())

    val gtx_clk   = Input(Clock())
    val gtx_clk90 = Input(Clock())
    val clk_200   = Input(Clock())

    // chip-facing TSI serial streams (Flipped mirrors chip-top direction)
    val chip0Serial = Flipped(new UDPTSISerialIO(params.serialWidth))
    val chip1Serial = Flipped(new UDPTSISerialIO(params.serialWidth))

    val chip0PhyLink = Input(Bool())
    val chip1PhyLink = Input(Bool())
    val selectedPhyLink = Output(Bool())
  })

  val mac = Module(new udp_tsi_top(params))

  mac.io.clk       := clock
  mac.io.rst       := reset.asBool
  mac.io.gtx_clk   := io.gtx_clk
  mac.io.gtx_clk90 := io.gtx_clk90
  mac.io.gtx_rst   := reset.asBool
  mac.io.clk_200   := io.clk_200

  // RGMII — single MAC drives the pad directly (no mux after ODDR)
  io.phyRgmii.txd    := mac.io.rgmii_txd
  io.phyRgmii.tx_ctl := mac.io.rgmii_tx_ctl
  io.phyRgmii.txc    := mac.io.rgmii_txc
  mac.io.rgmii_rxd    := io.phyRgmii.rxd
  mac.io.rgmii_rx_ctl := io.phyRgmii.rx_ctl
  mac.io.rgmii_rxc    := io.phyRgmii.rxc

  io.phyResetN := mac.io.phy_reset_n

  // Effective chip-select: io.select XOR'd with the ctrl-port-latched
  // select_invert bit from udp_tsi_top (CTRL_CMD_SET_SELECT_INVERT).
  val select = io.select ^ mac.io.select_invert

  // TX path: selected chip's serial.out → mac serial_in
  mac.io.serial_in_bits  := Mux(select, io.chip1Serial.out.bits,  io.chip0Serial.out.bits)
  mac.io.serial_in_valid := Mux(select, io.chip1Serial.out.valid, io.chip0Serial.out.valid)
  io.chip0Serial.out.ready := !select && mac.io.serial_in_ready
  io.chip1Serial.out.ready :=  select && mac.io.serial_in_ready

  // RX path: mac serial_out → selected chip's serial.in
  io.chip0Serial.in.bits  := mac.io.serial_out_bits
  io.chip1Serial.in.bits  := mac.io.serial_out_bits
  io.chip0Serial.in.valid := !select && mac.io.serial_out_valid
  io.chip1Serial.in.valid :=  select && mac.io.serial_out_valid
  mac.io.serial_out_ready := Mux(select, io.chip1Serial.in.ready, io.chip0Serial.in.ready)

  io.selectedPhyLink := Mux(select, io.chip1PhyLink, io.chip0PhyLink)
}
