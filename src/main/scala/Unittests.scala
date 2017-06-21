package testchipip

import chisel3._
import chisel3.util._
import config.Parameters
import diplomacy._
import unittest._
import uncore.devices._
import uncore.tilelink2._
import uncore.tilelink._
import uncore.converters._
import uncore.util._
import _root_.util._

//object TileLinkUnitTestUtils {
//  def fullDriverSet(sweepDepth: Int)(implicit p: Parameters) = {
//    val tlExternal = p(TLKey(p(TLId)))
//    val tlDataBeats = tlExternal.dataBeats
//    Module(new DriverSet(
//      (driverParams: Parameters) => {
//        implicit val p = driverParams
//        Seq(
//          Module(new PutSweepDriver(sweepDepth)),
//          Module(new PutMaskDriver),
//          Module(new PutAtomicDriver),
//          Module(new PutBlockSweepDriver(sweepDepth / tlDataBeats)),
//          Module(new PrefetchDriver),
//          Module(new GetMultiWidthDriver))
//      }))
//  }
//}
//
//class TileLinkSwitcherTest(implicit val p: Parameters)
//    extends UnitTest with HasTileLinkParameters {
//
//  def addrToRoute(addr: UInt): UInt = {
//    val lsb = tlByteAddrBits + tlBeatAddrBits
//    UIntToOH(addr(lsb, lsb))
//  }
//
//  val depth = 2 * tlDataBeats
//  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
//  driver.io.start := io.start
//  io.finished := driver.io.finished
//
//  val allowedRoutes = Seq(Seq(1), Seq(0))
//  val testrams = Seq.fill(2) { Module(new TileLinkTestRAM(depth)) }
//  val interconnect = Module(new TileLinkMemoryInterconnect(1, 2))
//  val switcher = Module(new ClientTileLinkIOSwitcher(2, 2, Some(allowedRoutes)))
//  val router = Module(new ClientUncachedTileLinkIORouter(2, addrToRoute _))
//  router.io.in <> driver.io.mem
//  switcher.io.in <> router.io.out.map(TileLinkIOWrapper(_))
//  interconnect.io.in <> switcher.io.out.map(TileLinkIOUnwrapper(_))
//  for ((ram, i) <- testrams.zipWithIndex) {
//    ram.io <> interconnect.io.out(i)
//  }
//  // swapsies
//  switcher.io.select(0) := 1.U
//  switcher.io.select(1) := 0.U
//}
//
//class UncachedTileLinkSwitcherTest(implicit val p: Parameters)
//    extends UnitTest with HasTileLinkParameters {
//
//  def addrToRoute(addr: UInt): UInt = {
//    val lsb = tlByteAddrBits + tlBeatAddrBits
//    UIntToOH(addr(lsb, lsb))
//  }
//
//  val depth = 2 * tlDataBeats
//  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
//  driver.io.start := io.start
//  io.finished := driver.io.finished
//
//  val allowedRoutes = Seq(Seq(1), Seq(0))
//  val testrams = Seq.fill(2) { Module(new TileLinkTestRAM(depth)) }
//  val interconnect = Module(new TileLinkMemoryInterconnect(1, 2))
//  val switcher = Module(new ClientUncachedTileLinkIOSwitcher(2, 2, Some(allowedRoutes)))
//  val router = Module(new ClientUncachedTileLinkIORouter(2, addrToRoute _))
//  router.io.in <> driver.io.mem
//  switcher.io.in <> router.io.out
//  interconnect.io.in <> switcher.io.out
//  for ((ram, i) <- testrams.zipWithIndex) {
//    ram.io <> interconnect.io.out(i)
//  }
//  // swapsies
//  switcher.io.select(0) := 1.U
//  switcher.io.select(1) := 0.U
//}
//
//class TileLinkSerdesTest(implicit val p: Parameters)
//    extends UnitTest with HasTileLinkParameters {
//  val serdesWidth = 8
//
//  val depth = 2 * tlDataBeats
//  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
//  driver.io.start := io.start
//  io.finished := driver.io.finished
//
//  val testram = Module(new TileLinkTestRAM(depth))
//
//  val serdes = Module(new ClientTileLinkIOSerdes(serdesWidth))
//  val desser = Module(new ClientTileLinkIODesser(serdesWidth))
//  serdes.io.tl <> TileLinkIOWrapper(driver.io.mem)
//  desser.io.serial.in <> serdes.io.serial.out
//  serdes.io.serial.in <> desser.io.serial.out
//  testram.io <> TileLinkIOUnwrapper(desser.io.tl)
//}
//
//class UncachedTileLinkSerdesTest(implicit val p: Parameters)
//    extends UnitTest with HasTileLinkParameters {
//  val serdesWidth = 8
//
//  val depth = 2 * tlDataBeats
//  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
//  driver.io.start := io.start
//  io.finished := driver.io.finished
//
//  val testram = Module(new TileLinkTestRAM(depth))
//
//  val serdes = Module(new ClientUncachedTileLinkIOSerdes(serdesWidth))
//  val desser = Module(new ClientUncachedTileLinkIODesser(serdesWidth))
//  serdes.io.tl <> driver.io.mem
//  desser.io.serial.in <> serdes.io.serial.out
//  serdes.io.serial.in <> desser.io.serial.out
//  testram.io <> desser.io.tl
//}
//
//class BidirectionalSerdesTest(implicit val p: Parameters)
//    extends UnitTest with HasTileLinkSerializerParameters {
//  val serdesWidth = 8
//  val tlSerialDataBeats = ((tlSerialDataBits - 1) / serdesWidth + 1) * tlDataBeats
//
//  val depth = 2 * tlDataBeats
//  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
//  driver.io.start := io.start
//  io.finished := driver.io.finished
//
//  val testram = Module(new TileLinkTestRAM(depth))
//
//  val serdes = Module(new ClientUncachedTileLinkIOBidirectionalSerdes(serdesWidth))
//  serdes.io.serial.in <> Queue(serdes.io.serial.out, tlSerialDataBeats)
//  testram.io <> TileLinkEnqueuer(serdes.io.tl_client, tlDataBeats)
//  serdes.io.tl_manager <> TileLinkEnqueuer(driver.io.mem, tlDataBeats)
//}
//
//class SCRFileTest(implicit val p: Parameters) extends UnitTest {
//  val scrBuilder = new SCRBuilder("scr")
//  scrBuilder.addStatus("stat")
//  scrBuilder.addControl("ctrl", 0.U)
//
//  val scr = scrBuilder.generate(0)
//  val tl = scr.io.tl
//  val stat = scr.status("stat")
//  val ctrl = scr.control("ctrl")
//
//  val s_idle :: s_stat_read :: s_ctrl_write :: s_finished :: Nil = Enum(4)
//  val state = Reg(init = s_idle)
//
//  val (stat_cnt, stat_done) = Counter(state === s_stat_read && tl.grant.fire(), 3)
//  stat := stat_cnt
//
//  val (ctrl_cnt, ctrl_done) = Counter(state === s_ctrl_write && tl.acquire.fire(), 3)
//
//  val sending = Reg(init = false.B)
//
//  when (state === s_idle && io.start) {
//    state := s_stat_read
//    sending := true.B
//  }
//  when (tl.acquire.fire()) { sending := false.B }
//  when (tl.grant.fire()) { sending := true.B }
//
//  when (stat_done) { state := s_ctrl_write }
//  when (ctrl_done) { state := s_finished }
//
//  tl.acquire.valid := sending && state.isOneOf(s_stat_read, s_ctrl_write)
//  tl.acquire.bits := Mux(state === s_stat_read,
//    Get(
//      client_xact_id = 0.U,
//      addr_block = 0.U,
//      addr_beat = 1.U),
//    Put(
//      client_xact_id = 0.U,
//      addr_block = 0.U,
//      addr_beat = 0.U,
//      data = ctrl_cnt + 1.U))
//  tl.grant.ready := !sending && state.isOneOf(s_stat_read, s_ctrl_write)
//  io.finished := (state === s_finished)
//
//  assert(!tl.grant.valid || state =/= s_stat_read ||
//         tl.grant.bits.data === stat_cnt, "Bad status value")
//  assert(state =/= s_ctrl_write || !sending || ctrl === ctrl_cnt,
//         "Bad ctrl value")
//}

class BlockDeviceTrackerTestDriver(nSectors: Int)(implicit p: Parameters)
    extends LazyModule with HasBlockDeviceParameters {
  val node = TLClientNode(TLClientParameters(
    name = "blkdev-testdriver", sourceId = IdRange(0, 1)))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val start = Input(Bool())
      val finished = Output(Bool())
      val front = new BlockDeviceTrackerIO
      val mem = node.bundleOut
    })

    val req = io.front.req
    val complete = io.front.complete
    val tl = io.mem.head
    val edge = node.edgesOut(0)

    val (s_start :: s_bdev_write_req :: s_bdev_write_complete ::
         s_bdev_read_req :: s_bdev_read_complete ::
         s_mem_read_req :: s_mem_read_resp :: s_done :: Nil) = Enum(8)
    val state = Reg(init = s_start)

    when (io.start && state === s_start) { state := s_bdev_write_req }
    when (state === s_bdev_write_req && req.ready) {
      state := s_bdev_write_complete
    }
    when (state === s_bdev_write_complete && complete.valid) {
      state := s_bdev_read_req
    }
    when (state === s_bdev_read_req && req.ready) {
      state := s_bdev_read_complete
    }
    when (state === s_bdev_read_complete && complete.valid) {
      state := s_mem_read_req
    }

    when (tl.a.fire()) { state := s_mem_read_resp }
    val (read_beat, read_sector_done) = Counter(tl.d.fire(), dataBeats)
    val (read_sector, read_all_done) = Counter(read_sector_done, nSectors)
    when (read_sector_done) { state := s_mem_read_req }
    when (read_all_done) { state := s_done }

    req.valid := state.isOneOf(s_bdev_write_req, s_bdev_read_req)
    req.bits.addr := Mux(state === s_bdev_write_req, 0x10000.U, 0x0.U)
    req.bits.offset := 0.U
    req.bits.len := nSectors.U
    req.bits.write := state === s_bdev_write_req
    complete.ready := state.isOneOf(s_bdev_write_complete, s_bdev_read_complete)

    val dataSize = log2Ceil(dataBytes)
    tl.a.valid := state === s_mem_read_req
    tl.a.bits := edge.Get(0.U, read_sector << dataSize.U, dataSize.U)._2
    tl.d.ready := state === s_mem_read_resp

    tl.b.ready := false.B
    tl.c.valid := false.B
    tl.e.valid := false.B

    io.finished := state === s_done

    val full_beat = Cat(read_sector, read_beat)
    val expected_data = (full_beat << 16.U) | full_beat
    assert(!tl.d.valid || tl.d.bits.data === expected_data,
      "Unexpected data read\n")
  }
}

class BlockDeviceTrackerTest(implicit p: Parameters) extends LazyModule
    with HasBlockDeviceParameters {
  val nSectors = 4
  val beatBytes = dataBitsPerBeat / 8

  val testData = Seq.tabulate(nSectors * dataBeats)(i => (i << 16) | i)
  val testBytes = testData.flatMap(
    i => (0 until beatBytes).map(
      j => ((i >> (j * 8)) & 0xff).toByte))

  val testram = LazyModule(new TLTestRAM(
    address = AddressSet(0x0, 0xffff),
    beatBytes = beatBytes))
  val testrom = LazyModule(new TLROM(
    0x10000, 64 * dataBytes, testBytes,
    beatBytes = beatBytes))

  val tracker = LazyModule(new BlockDeviceTracker(0))
  val driver = LazyModule(new BlockDeviceTrackerTestDriver(nSectors))
  val xbar = LazyModule(new TLXbar)

  xbar.node := driver.node
  xbar.node := tracker.node
  testram.node := TLBuffer()(TLFragmenter(beatBytes, dataBytes)(xbar.node))
  testrom.node := TLBuffer()(TLFragmenter(beatBytes, dataBytes)(xbar.node))

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    val blkdev = Module(new BlockDeviceModel(nSectors))
    blkdev.io <> tracker.module.io.bdev
    tracker.module.io.front <> driver.module.io.front
    driver.module.io.start := io.start
    io.finished := driver.module.io.finished
  }
}

class BlockDeviceTrackerTestWrapper(implicit p: Parameters) extends UnitTest {
  val testParams = p.alterPartial({
    case BlockDeviceKey => BlockDeviceConfig(dataBitsPerBeat = 32)
  })
  val test = Module(LazyModule(
    new BlockDeviceTrackerTest()(testParams)).module)
  test.io.start := io.start
  io.finished := test.io.finished
}

object TestChipUnitTests {
  def apply(implicit p: Parameters): Seq[UnitTest] =
    Seq(
    //  Module(new TileLinkSwitcherTest),
    //  Module(new UncachedTileLinkSwitcherTest),
    //  Module(new TileLinkSerdesTest),
    //  Module(new UncachedTileLinkSerdesTest),
    //  Module(new BidirectionalSerdesTest),
    //  Module(new SCRFileTest),
      Module(new BlockDeviceTrackerTestWrapper))
}
