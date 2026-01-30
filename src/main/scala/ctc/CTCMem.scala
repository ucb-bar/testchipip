package testchipip.ctc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import testchipip.serdes.{DecoupledPhitIO, DecoupledSerialPhy}
import testchipip.ctc._

// A test memory like SerialRAM but for CTC
// Takes in DecoupledFlitIO, puts it through CTCToTileLink, then connects the TileLink node to TLRAM
class CTCMem(params: CTCParams)(implicit p: Parameters) extends LazyModule {
    val beatBytes = 8
    val ctc2tl = LazyModule(new CTCToTileLink(sourceIds = 1, portId = 0))

    val xbar = TLXbar()

    // Use smaller memory size for testing
    val testmems = params.offchip.map(address => LazyModule(new TLRAM(address = AddressSet(address.base, 0xFFFFFL), beatBytes = beatBytes) {override lazy val desiredName = "CTCMem"}))
    testmems.foreach { s => (s.node
      := TLBuffer()
      := TLFragmenter(beatBytes, p(CacheBlockBytes), nameSuffix = Some("CTCMem"))
      := xbar)
    }

    xbar := ctc2tl.node

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {
        val io = IO(new Bundle {
            val ser = new DecoupledPhitIO(CTC.OUTER_WIDTH)
        })

        val phy = Module(new DecoupledSerialPhy(2, params.phyParams.get))
        phy.io.outer_clock := clock
        phy.io.outer_reset := reset
        phy.io.inner_clock := clock
        phy.io.inner_reset := reset
        phy.io.outer_ser <> io.ser
        phy.io.inner_ser(0).in <> ctc2tl.module.io.flit.in
        phy.io.inner_ser(1).out <> ctc2tl.module.io.flit.out

        // Tie off unused channels
        phy.io.inner_ser(1).in.ready := false.B
        phy.io.inner_ser(0).out.valid := false.B
        phy.io.inner_ser(0).out.bits := DontCare
    }
}