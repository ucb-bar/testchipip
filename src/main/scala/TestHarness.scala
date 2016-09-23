package testchipip

import Chisel._
import rocketchip._
import cde.Parameters
import uncore.tilelink._
import uncore.converters._
import rocketchip.GlobalAddrMap

class TestHarness(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }
  val dut = p(BuildExampleTop)(p).module

  require(dut.io.mem_clk.isEmpty)
  require(dut.io.mem_rst.isEmpty)
  require(dut.io.mem_ahb.isEmpty)
  require(dut.io.mem_axi.isEmpty)
  require(dut.io.bus_clk.isEmpty)
  require(dut.io.bus_rst.isEmpty)
  require(dut.io.bus_axi.isEmpty)
  require(dut.io.mmio_clk.isEmpty)
  require(dut.io.mmio_rst.isEmpty)
  require(dut.io.mmio_ahb.isEmpty)
  require(dut.io.mmio_tl.isEmpty)
  require(dut.io.mmio_axi.isEmpty)

  for (int <- dut.io.interrupts)
    int := Bool(false)

  if (!p(IncludeJtagDTM)) {
    // Todo: enable the usage of different clocks
    // to test the synchronizer more aggressively.
    val dtm_clock = clock
    val dtm_reset = reset
    if (dut.io.debug_clk.isDefined) dut.io.debug_clk.get := dtm_clock
    if (dut.io.debug_rst.isDefined) dut.io.debug_rst.get := dtm_reset
    val dtm = Module(new SimDTM).connect(dtm_clock, dtm_reset, dut.io.debug.get,
      dut.io.success, io.success)
  } else {
    val jtag = Module(new JTAGVPI).connect(dut.io.jtag.get, reset, io.success)
  }

  def connectMem(mem_tl: Vec[ClientUncachedTileLinkIO])(implicit p: Parameters) {
    val nChannels = mem_tl.size
    val switcher = Module(new ClientUncachedTileLinkIOSwitcher(nChannels, nChannels))
    if (nChannels == 1) {
      switcher.io.select(0) := UInt(0)
    } else {
      switcher.io.select.zipWithIndex.foreach { case (select, i) =>
        select := ~UInt(i, log2Up(nChannels))
      }
    }
    switcher.io.in <> ChannelAddressMapper.unmap(mem_tl)
    val serdesser = switcher.io.out.map { out =>
      val serdes = Module(new ClientUncachedTileLinkIOSerdes(8))
      val desser = Module(new ClientUncachedTileLinkIODesser(8))
      serdes.io.tl <> out
      desser.io.serial.in <> serdes.io.serial.out
      serdes.io.serial.in <> desser.io.serial.out
      desser.io.tl
    }
    val mapped = ChannelAddressMapper.map(serdesser)
    val memSize = p(GlobalAddrMap).get("mem").size
    for (chan <- mapped) {
      val mem = Module(new SimAXIMem(memSize / nChannels))
      mem.io.axi <> PeripheryUtils.convertTLtoAXI(chan)
    }
  }

  if (dut.io.mem_tl.nonEmpty) {
    connectMem(dut.io.mem_tl)(p.alterPartial({case TLId => "Outermost"}))
  }
}
