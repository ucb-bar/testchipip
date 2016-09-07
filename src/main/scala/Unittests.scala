package testchipip

import Chisel._
import junctions.unittests._
import uncore.unittests._
import uncore.devices._
import uncore.tilelink._
import uncore.converters._
import uncore.util._
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

  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := io.start
  io.finished := driver.io.finished

  val depth = 2 * tlDataBeats
  val testrams = Seq.fill(2) { Module(new TileLinkTestRAM(depth)) }
  val interconnect = Module(new TileLinkMemoryInterconnect(1, 2))
  val switcher = Module(new ClientTileLinkIOSwitcher(2, 2))
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

  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := io.start
  io.finished := driver.io.finished

  val depth = 2 * tlDataBeats
  val testrams = Seq.fill(2) { Module(new TileLinkTestRAM(depth)) }
  val interconnect = Module(new TileLinkMemoryInterconnect(1, 2))
  val switcher = Module(new ClientUncachedTileLinkIOSwitcher(2, 2))
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

  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := io.start
  io.finished := driver.io.finished

  val depth = 2 * tlDataBeats
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

  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := io.start
  io.finished := driver.io.finished

  val depth = 2 * tlDataBeats
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

  val driver = TileLinkUnitTestUtils.fullDriverSet(depth)
  driver.io.start := io.start
  io.finished := driver.io.finished

  val depth = 2 * tlDataBeats
  val testram = Module(new TileLinkTestRAM(depth))

  val serdes = Module(new ClientUncachedTileLinkIOBidirectionalSerdes(serdesWidth))
  serdes.io.serial.in <> Queue(serdes.io.serial.out, tlSerialDataBeats)
  testram.io <> ClientUncachedTileLinkEnqueuer(serdes.io.tl_client, tlDataBeats)
  serdes.io.tl_manager <> ClientUncachedTileLinkEnqueuer(driver.io.mem, tlDataBeats)
}

object TestChipUnitTests {
  def apply(implicit p: Parameters): Seq[UnitTest] =
    Seq(
      Module(new TileLinkSwitcherTest),
      Module(new UncachedTileLinkSwitcherTest),
      Module(new TileLinkSerdesTest),
      Module(new UncachedTileLinkSerdesTest),
      Module(new BidirectionalSerdesTest))
}
