package testchipip.tsi

import chisel3._
import testchipip.tsi.RGMIIPort

class UDPTSIRGMIIMuxShim extends Module {
  val io = IO(new Bundle {
    val select = Input(Bool())

    // Physical RGMII/PHY-facing interface.
    val phyRgmii = new RGMIIPort
    val phyResetN = Output(Bool())

    // Chip-facing interfaces.
    val chip0Rgmii = Flipped(new RGMIIPort)
    val chip0PhyResetN = Input(Bool())
    val chip0PhyLink = Input(Bool())

    val chip1Rgmii = Flipped(new RGMIIPort)
    val chip1PhyResetN = Input(Bool())
    val chip1PhyLink = Input(Bool())

    // Selected status for debug/visibility.
    val selectedPhyLink = Output(Bool())
  })

  val useChip1 = io.select

  io.phyRgmii.txd := Mux(useChip1, io.chip1Rgmii.txd, io.chip0Rgmii.txd)
  io.phyRgmii.tx_ctl := Mux(useChip1, io.chip1Rgmii.tx_ctl, io.chip0Rgmii.tx_ctl)
  io.phyRgmii.txc := Mux(useChip1, io.chip1Rgmii.txc, io.chip0Rgmii.txc)

  // RX clock is broadcast to both chips; data/control are qualified by selection.
  io.chip0Rgmii.rxc := io.phyRgmii.rxc
  io.chip1Rgmii.rxc := io.phyRgmii.rxc
  io.chip0Rgmii.rxd := Mux(useChip1, 0.U, io.phyRgmii.rxd)
  io.chip1Rgmii.rxd := Mux(useChip1, io.phyRgmii.rxd, 0.U)
  io.chip0Rgmii.rx_ctl := Mux(useChip1, false.B, io.phyRgmii.rx_ctl)
  io.chip1Rgmii.rx_ctl := Mux(useChip1, io.phyRgmii.rx_ctl, false.B)

  io.phyResetN := Mux(useChip1, io.chip1PhyResetN, io.chip0PhyResetN)
  io.selectedPhyLink := Mux(useChip1, io.chip1PhyLink, io.chip0PhyLink)
}
