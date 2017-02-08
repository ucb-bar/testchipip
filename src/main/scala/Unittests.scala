package testchipip

import chisel3._
import chisel3.util._
import unittest._
import uncore.devices._
import uncore.tilelink._
import uncore.converters._
import uncore.util._
import _root_.util._
import cde.Parameters

object TileLinkUnitTestUtils {
  def fullDriverSet(sweepDepth: Int)(implicit p: Parameters) = {
    val tlExternal = p(TLKey(p(TLId)))
    val tlDataBeats = tlExternal.dataBeats
    Module(new DriverSet(
      (driverParams: Parameters) => {
        implicit val p = driverParams
        Seq(
          Module(new PutSweepDriver(sweepDepth)),
          Module(new PutMaskDriver),
          Module(new PutAtomicDriver),
          Module(new PutBlockSweepDriver(sweepDepth / tlDataBeats)),
          Module(new PrefetchDriver),
          Module(new GetMultiWidthDriver))
      }))
  }
}

class TileLinkSwitcherTest(implicit val p: Parameters)
    extends UnitTest with HasTileLinkParameters {

  def addrToRoute(addr: UInt): UInt = {
    val lsb = tlByteAddrBits + tlBeatAddrBits
    UIntToOH(addr(lsb, lsb))
  }

  val depth = 2 * tlDataBeats
  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := io.start
  io.finished := driver.io.finished

  val allowedRoutes = Seq(Seq(1), Seq(0))
  val testrams = Seq.fill(2) { Module(new TileLinkTestRAM(depth)) }
  val interconnect = Module(new TileLinkMemoryInterconnect(1, 2))
  val switcher = Module(new ClientTileLinkIOSwitcher(2, 2, Some(allowedRoutes)))
  val router = Module(new ClientUncachedTileLinkIORouter(2, addrToRoute _))
  router.io.in <> driver.io.mem
  switcher.io.in <> router.io.out.map(TileLinkIOWrapper(_))
  interconnect.io.in <> switcher.io.out.map(TileLinkIOUnwrapper(_))
  for ((ram, i) <- testrams.zipWithIndex) {
    ram.io <> interconnect.io.out(i)
  }
  // swapsies
  switcher.io.select(0) := UInt(1)
  switcher.io.select(1) := UInt(0)
}

class UncachedTileLinkSwitcherTest(implicit val p: Parameters)
    extends UnitTest with HasTileLinkParameters {

  def addrToRoute(addr: UInt): UInt = {
    val lsb = tlByteAddrBits + tlBeatAddrBits
    UIntToOH(addr(lsb, lsb))
  }

  val depth = 2 * tlDataBeats
  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := io.start
  io.finished := driver.io.finished

  val allowedRoutes = Seq(Seq(1), Seq(0))
  val testrams = Seq.fill(2) { Module(new TileLinkTestRAM(depth)) }
  val interconnect = Module(new TileLinkMemoryInterconnect(1, 2))
  val switcher = Module(new ClientUncachedTileLinkIOSwitcher(2, 2, Some(allowedRoutes)))
  val router = Module(new ClientUncachedTileLinkIORouter(2, addrToRoute _))
  router.io.in <> driver.io.mem
  switcher.io.in <> router.io.out
  interconnect.io.in <> switcher.io.out
  for ((ram, i) <- testrams.zipWithIndex) {
    ram.io <> interconnect.io.out(i)
  }
  // swapsies
  switcher.io.select(0) := UInt(1)
  switcher.io.select(1) := UInt(0)
}

class TileLinkSerdesTest(implicit val p: Parameters)
    extends UnitTest with HasTileLinkParameters {
  val serdesWidth = 8

  val depth = 2 * tlDataBeats
  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := io.start
  io.finished := driver.io.finished

  val testram = Module(new TileLinkTestRAM(depth))

  val serdes = Module(new ClientTileLinkIOSerdes(serdesWidth))
  val desser = Module(new ClientTileLinkIODesser(serdesWidth))
  serdes.io.tl <> TileLinkIOWrapper(driver.io.mem)
  desser.io.serial.in <> serdes.io.serial.out
  serdes.io.serial.in <> desser.io.serial.out
  testram.io <> TileLinkIOUnwrapper(desser.io.tl)
}

class UncachedTileLinkSerdesTest(implicit val p: Parameters)
    extends UnitTest with HasTileLinkParameters {
  val serdesWidth = 8

  val depth = 2 * tlDataBeats
  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := io.start
  io.finished := driver.io.finished

  val testram = Module(new TileLinkTestRAM(depth))

  val serdes = Module(new ClientUncachedTileLinkIOSerdes(serdesWidth))
  val desser = Module(new ClientUncachedTileLinkIODesser(serdesWidth))
  serdes.io.tl <> driver.io.mem
  desser.io.serial.in <> serdes.io.serial.out
  serdes.io.serial.in <> desser.io.serial.out
  testram.io <> desser.io.tl
}

class BidirectionalSerdesTest(implicit val p: Parameters)
    extends UnitTest with HasTileLinkSerializerParameters {
  val serdesWidth = 8
  val tlSerialDataBeats = ((tlSerialDataBits - 1) / serdesWidth + 1) * tlDataBeats

  val depth = 2 * tlDataBeats
  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := io.start
  io.finished := driver.io.finished

  val testram = Module(new TileLinkTestRAM(depth))

  val serdes = Module(new ClientUncachedTileLinkIOBidirectionalSerdes(serdesWidth))
  serdes.io.serial.in <> Queue(serdes.io.serial.out, tlSerialDataBeats)
  testram.io <> TileLinkEnqueuer(serdes.io.tl_client, tlDataBeats)
  serdes.io.tl_manager <> TileLinkEnqueuer(driver.io.mem, tlDataBeats)
}

class SCRFileTest(implicit val p: Parameters) extends UnitTest {
  val scrBuilder = new SCRBuilder("scr")
  scrBuilder.addStatus("stat")
  scrBuilder.addControl("ctrl", UInt(0))

  val scr = scrBuilder.generate(0)
  val tl = scr.io.tl
  val stat = scr.status("stat")
  val ctrl = scr.control("ctrl")

  val s_idle :: s_stat_read :: s_ctrl_write :: s_finished :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  val (stat_cnt, stat_done) = Counter(state === s_stat_read && tl.grant.fire(), 3)
  stat := stat_cnt

  val (ctrl_cnt, ctrl_done) = Counter(state === s_ctrl_write && tl.acquire.fire(), 3)

  val sending = Reg(init = Bool(false))

  when (state === s_idle && io.start) {
    state := s_stat_read
    sending := Bool(true)
  }
  when (tl.acquire.fire()) { sending := Bool(false) }
  when (tl.grant.fire()) { sending := Bool(true) }

  when (stat_done) { state := s_ctrl_write }
  when (ctrl_done) { state := s_finished }

  tl.acquire.valid := sending && state.isOneOf(s_stat_read, s_ctrl_write)
  tl.acquire.bits := Mux(state === s_stat_read,
    Get(
      client_xact_id = UInt(0),
      addr_block = UInt(0),
      addr_beat = UInt(1)),
    Put(
      client_xact_id = UInt(0),
      addr_block = UInt(0),
      addr_beat = UInt(0),
      data = ctrl_cnt + UInt(1)))
  tl.grant.ready := !sending && state.isOneOf(s_stat_read, s_ctrl_write)
  io.finished := (state === s_finished)

  assert(!tl.grant.valid || state =/= s_stat_read ||
         tl.grant.bits.data === stat_cnt, "Bad status value")
  assert(state =/= s_ctrl_write || !sending || ctrl === ctrl_cnt,
         "Bad ctrl value")
}

object TestChipUnitTests {
  def apply(implicit p: Parameters): Seq[UnitTest] =
    Seq(
      Module(new TileLinkSwitcherTest),
      Module(new UncachedTileLinkSwitcherTest),
      Module(new TileLinkSerdesTest),
      Module(new UncachedTileLinkSerdesTest),
      Module(new BidirectionalSerdesTest),
      Module(new SCRFileTest))
}
