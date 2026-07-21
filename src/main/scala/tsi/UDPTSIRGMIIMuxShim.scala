package testchipip.tsi

import chisel3._
import chisel3.util._

// ============================================================================
// Fast TileLink port exposed by the write router (harness attaches it to DDR).
// Field layout/names must stay in sync with tsi_fastpath_write_router.v and
// the harness-side connection.
// ============================================================================

class FastTLA(params: UDPTSIParams) extends Bundle {
  val opcode  = UInt(3.W)
  val size    = UInt(3.W)
  val source  = UInt(params.fastTLSourceBits.W)
  val address = UInt(params.fastTLAddrBits.W)
  val mask    = UInt((params.fastTLDataBits / 8).W)
  val data    = UInt(params.fastTLDataBits.W)
  val corrupt = Bool()
}

class FastTLD(params: UDPTSIParams) extends Bundle {
  val source  = UInt(params.fastTLSourceBits.W)
  val data    = UInt(params.fastTLDataBits.W)
  val denied  = Bool()
  val corrupt = Bool()
}

class FastTLIO(params: UDPTSIParams) extends Bundle {
  val a = Decoupled(new FastTLA(params))            // master A out
  val d = Flipped(Decoupled(new FastTLD(params)))   // response D in
}

// ============================================================================
// UDPTSIStreamMuxShim
//
// Instantiates a single udp_tsi_top MAC plus the tsi_fastpath_write_router, and
// muxes two chips' TSI serial streams behind the router.  A single MAC drives
// the RGMII pads directly (ODDR → OBUF, satisfying Vivado DRC REQP-1884); the
// per-chip selection happens on the serial streams, not the pins.
//
// The router services fastpath-window transactions on its own multi-outstanding
// TileLink master (io.fast_tl, attached to DDR in the harness) and replays
// everything else to the selected chip's legacy TSI backend.
// ============================================================================

class UDPTSIStreamMuxShim(params: UDPTSIParams) extends Module {
  val io = IO(new Bundle {
    val select = Input(Bool())

    val phyRgmii  = new RGMIIPort
    val phyResetN = Output(Bool())
    val phyLinkUp = Output(Bool())

    val gtx_clk   = Input(Clock())
    val gtx_clk90 = Input(Clock())
    val clk_200   = Input(Clock())

    // Fast TileLink port to DDR (attached in the harness)
    val fastActive = Output(Bool())
    val fast_tl    = new FastTLIO(params)

    // chip-facing TSI serial streams (Flipped mirrors chip-top direction)
    val chip0Serial = Flipped(new UDPTSISerialIO(params.serialWidth))
    val chip1Serial = Flipped(new UDPTSISerialIO(params.serialWidth))

    // Host level-hold chip reset (OR'd into the chip reset button in the harness)
    val chipReset = Output(UInt(2.W))

    // FPGA SW reset pulse (CTRL_CMD_FPGA_RESET): auto-release. The harness fans
    // this into the chip + DDR-fabric resets (NOT the MAC/PHY, NOT the MIG).
    val fpgaReset = Output(Bool())
  })

  val mac    = Module(new udp_tsi_top(params))
  val router = Module(new tsi_fastpath_write_router(params))

  // ---- MAC: clocks / reset / pads ----
  mac.io.clk       := clock
  mac.io.rst       := reset.asBool
  mac.io.gtx_clk   := io.gtx_clk
  mac.io.gtx_clk90 := io.gtx_clk90
  mac.io.gtx_rst   := reset.asBool
  mac.io.clk_200   := io.clk_200

  io.phyRgmii.txd    := mac.io.rgmii_txd
  io.phyRgmii.tx_ctl := mac.io.rgmii_tx_ctl
  io.phyRgmii.txc    := mac.io.rgmii_txc
  mac.io.rgmii_rxd    := io.phyRgmii.rxd
  mac.io.rgmii_rx_ctl := io.phyRgmii.rx_ctl
  mac.io.rgmii_rxc    := io.phyRgmii.rxc

  io.phyResetN := mac.io.phy_reset_n
  io.phyLinkUp := mac.io.phy_link_up

  // MAC UART unused here (host does PHY MDIO over a separate UART); tie rx high.
  mac.io.uart_rx := true.B

  // ---- Chip select: raw switch in, recency-mux-resolved value out ----
  mac.io.select_switch := io.select
  val select = mac.io.select_resolved

  io.chipReset := mac.io.chip_reset
  io.fpgaReset := mac.io.fpga_sw_reset

  // ---- MAC serial <-> router ----
  router.io.clock := clock
  // The SW-reset pulse also resets the router FSM/FIFOs (clears a wedged
  // outstanding_reg / RX FIFO) while leaving the always-up MAC/PHY alone.
  router.io.reset := reset.asBool | mac.io.fpga_sw_reset
  router.io.fastpath_base := mac.io.fastpath_base
  router.io.fastpath_size := mac.io.fastpath_size

  router.io.tsi_in_valid  := mac.io.serial_out_valid
  router.io.tsi_in_bits   := mac.io.serial_out_bits
  mac.io.serial_out_ready := router.io.tsi_in_ready

  mac.io.serial_in_valid := router.io.tsi_out_valid
  mac.io.serial_in_bits  := router.io.tsi_out_bits
  router.io.tsi_out_ready := mac.io.serial_in_ready

  io.fastActive := router.io.fast_active

  // ---- Router legacy backend <-> selected chip serial streams ----
  // Commands (router -> chip.in)
  io.chip0Serial.in.bits  := router.io.legacy_tsi_in_bits
  io.chip1Serial.in.bits  := router.io.legacy_tsi_in_bits
  io.chip0Serial.in.valid := !select && router.io.legacy_tsi_in_valid
  io.chip1Serial.in.valid :=  select && router.io.legacy_tsi_in_valid
  router.io.legacy_tsi_in_ready := Mux(select, io.chip1Serial.in.ready, io.chip0Serial.in.ready)

  // Responses (chip.out -> router)
  router.io.legacy_tsi_out_valid := Mux(select, io.chip1Serial.out.valid, io.chip0Serial.out.valid)
  router.io.legacy_tsi_out_bits  := Mux(select, io.chip1Serial.out.bits,  io.chip0Serial.out.bits)
  io.chip0Serial.out.ready := !select && router.io.legacy_tsi_out_ready
  io.chip1Serial.out.ready :=  select && router.io.legacy_tsi_out_ready

  // ---- Router fast TileLink master <-> shim IO ----
  io.fast_tl.a.valid        := router.io.fast_a_valid
  router.io.fast_a_ready    := io.fast_tl.a.ready
  io.fast_tl.a.bits.opcode  := router.io.fast_a_opcode
  io.fast_tl.a.bits.size    := router.io.fast_a_size
  io.fast_tl.a.bits.source  := router.io.fast_a_source
  io.fast_tl.a.bits.address := router.io.fast_a_address
  io.fast_tl.a.bits.mask    := router.io.fast_a_mask
  io.fast_tl.a.bits.data    := router.io.fast_a_data
  io.fast_tl.a.bits.corrupt := router.io.fast_a_corrupt

  router.io.fast_d_valid   := io.fast_tl.d.valid
  io.fast_tl.d.ready       := router.io.fast_d_ready
  router.io.fast_d_source  := io.fast_tl.d.bits.source
  router.io.fast_d_data    := io.fast_tl.d.bits.data
  router.io.fast_d_denied  := io.fast_tl.d.bits.denied
  router.io.fast_d_corrupt := io.fast_tl.d.bits.corrupt
}
