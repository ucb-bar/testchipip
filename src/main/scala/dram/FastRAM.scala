package testchipip.dram

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.subsystem._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLToAXI4
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import testchipip.tsi.SerialTLROM
import testchipip.serdes._
import testchipip.dram.SimDRAM

// SerialRAM that uses SimDRAM instead of TLRAM
// Simulates significantly faster even with the overhead of translating TL-> AXI4

class FastRAM(tl_serdesser: TLSerdesser, params: SerialTLParams, chipId: Int = 0)(implicit p: Parameters) extends LazyModule {
  val managerParams = tl_serdesser.module.client_edge.map(_.slave)
  val clientParams = tl_serdesser.module.manager_edge.map(_.master)
  val serdesser = LazyModule(new TLSerdesser(
    tl_serdesser.flitWidth,
    clientParams,
    managerParams,
    tl_serdesser.bundleParams,
    nameSuffix = Some("FastRAM")
  ))

  val beatBytes = 8

  // Manager side: TSI → TSIToTileLink → serdesser (sends HTIF commands to chip)
  val tsi2tl = serdesser.managerNode.map { _ =>
    val tsi2tl = LazyModule(new testchipip.tsi.TSIToTileLink)
    serdesser.managerNode.get := TLBuffer() := tsi2tl.node
    tsi2tl
  }

  // Client side: chip memory requests come through serdesser → xbar → {ROM, SimDRAM via AXI4}
  val memAXI4Node = serdesser.clientNode.map { clientNode =>
    val memParams = params.manager.get.memParams
    val romParams = params.manager.get.romParams

    val xbar = TLXbar()

    // ROM stays as TileLink
    val rom = romParams.map { romParams => SerialTLROM(romParams, beatBytes) }
    rom.foreach { r => (r.node
      := TLFragmenter(beatBytes, p(CacheBlockBytes), nameSuffix = Some("FastRAM_ROM"))
      := xbar)
    }

    // Memory: TL → AXI4 → AXI4SlaveNode (wired to SimDRAM in module)
    val memNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      slaves = memParams.map { mem =>
        AXI4SlaveParameters(
          address = AddressSet.misaligned(mem.address, mem.size),
          supportsWrite = TransferSizes(1, p(CacheBlockBytes)),
          supportsRead = TransferSizes(1, p(CacheBlockBytes)),
          interleavedId = Some(0)
        )
      },
      beatBytes = beatBytes
    )))

    memNode := AXI4Buffer() := AXI4UserYanker() := TLToAXI4() := TLWidthWidget(beatBytes) := xbar

    xbar := clientNode
    memNode
  }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ser = new DecoupledPhitIO(params.phyParams.phitWidth)
      val tsi = tsi2tl.map(_ => new testchipip.tsi.TSIIO)
      val tsi2tl_state = Output(UInt())
    })

    val phy = Module(new DecoupledSerialPhy(5, params.phyParams))
    phy.io.outer_clock := clock
    phy.io.outer_reset := reset
    phy.io.inner_clock := clock
    phy.io.inner_reset := reset
    phy.io.outer_ser <> io.ser
    for (i <- 0 until 5) {
      serdesser.module.io.ser(i) <> phy.io.inner_ser(i)
    }
    io.tsi.foreach(_ <> tsi2tl.get.module.io.tsi)
    io.tsi2tl_state := tsi2tl.map(_.module.io.state).getOrElse(0.U(1.W))

    // Wire AXI4SlaveNode to SimDRAM BlackBox(es)
    memAXI4Node.foreach { node =>
      val (axi, edge) = node.in(0)
      val memParams = params.manager.get.memParams

      // For a single memory region, connect directly
      // For multiple, we'd need an AXI4 xbar (typically there's just one)
      require(memParams.size == 1, "FastRAM currently supports a single memory region")
      val mem = memParams.head
      val simdram = Module(new SimDRAM(
        memSize = mem.size,
        lineSize = p(CacheBlockBytes),
        clockFreqHz = BigInt(100000000),
        memBase = mem.address,
        params = edge.bundle,
        chipId = chipId
      ))
      simdram.io.clock := clock
      simdram.io.reset := reset
      simdram.io.axi <> axi
    }

    require(serdesser.module.mergedParams == tl_serdesser.module.mergedParams,
      "Mismatch between chip-side diplomatic params and harness-side diplomatic params:\n" +
        s"Harness-side params: ${serdesser.module.mergedParams}\n" +
        s"Chip-side params: ${tl_serdesser.module.mergedParams}")
  }
}
