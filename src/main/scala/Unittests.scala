package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.devices.tilelink.{TLTestRAM, TLROM, TLError, ErrorParams}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest._
import freechips.rocketchip.util._

class BlockDeviceTrackerTestDriver(nSectors: Int)(implicit p: Parameters)
    extends LazyModule with HasBlockDeviceParameters {
  val node = TLHelper.makeClientNode(
    name = "blkdev-testdriver", sourceId = IdRange(0, 1))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val start = Input(Bool())
      val finished = Output(Bool())
      val front = new BlockDeviceTrackerIO
    })

    val req = io.front.req
    val complete = io.front.complete
    val (tl, edge) = node.out(0)

    val (s_start :: s_bdev_write_req :: s_bdev_write_complete ::
         s_bdev_read_req :: s_bdev_read_complete ::
         s_mem_read_req :: s_mem_read_resp :: s_done :: Nil) = Enum(8)
    val state = RegInit(s_start)

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

    val beatBytes = dataBitsPerBeat / 8
    val full_beat = Wire(UInt(8.W), init = Cat(read_sector, read_beat))
    val expected_data = Fill(beatBytes, full_beat)

    assert(!tl.d.valid || tl.d.bits.data === expected_data,
      "Unexpected data read\n")
  }
}

class BlockDeviceTrackerTest(implicit p: Parameters) extends LazyModule
    with HasBlockDeviceParameters {
  val nSectors = 4
  val beatBytes = dataBitsPerBeat / 8

  val testBytes = Seq.tabulate(nSectors * dataBeats)(
    i => Seq.fill(beatBytes) { i.toByte }).flatten

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
  testram.node := TLBuffer() := TLFragmenter(beatBytes, dataBytes) := xbar.node
  testrom.node := TLBuffer() := TLFragmenter(beatBytes, dataBytes) := xbar.node

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    val io = IO(new Bundle with UnitTestIO)
    val blkdev = Module(new BlockDeviceModel(nSectors))
    blkdev.io <> tracker.module.io.bdev
    tracker.module.io.front <> driver.module.io.front
    driver.module.io.start := io.start
    io.finished := driver.module.io.finished
  }
}

class BlockDeviceTrackerTestWrapper(implicit p: Parameters) extends UnitTest {
  val testParams = p.alterPartial({
    case BlockDeviceKey => BlockDeviceConfig()
  })
  val test = Module(LazyModule(
    new BlockDeviceTrackerTest()(testParams)).module)
  test.io.start := io.start
  io.finished := test.io.finished
}

class SerdesTest(implicit p: Parameters) extends LazyModule {
  val idBits = 2
  val beatBytes = 8
  val lineBytes = 64
  val serWidth = 32

  val fuzzer = LazyModule(new TLFuzzer(
    nOperations = 32,
    inFlight = 1 << idBits))

  val serdes = LazyModule(new TLSerdes(
    w = serWidth,
    params = Seq(TLManagerParameters(
      address = Seq(AddressSet(0, 0xffff)),
      regionType = RegionType.UNCACHED,
      supportsGet = TransferSizes(1, lineBytes),
      supportsPutFull = TransferSizes(1, lineBytes))),
    beatBytes = beatBytes))

  val desser = LazyModule(new TLDesser(
    w = serWidth,
    params = Seq(TLClientParameters(
      name = "tl-desser",
      sourceId = IdRange(0, 1 << idBits)))))

  val testram = LazyModule(new TLTestRAM(
    address = AddressSet(0, 0xffff),
    beatBytes = beatBytes))

  serdes.node := TLBuffer() := fuzzer.node
  testram.node := TLBuffer() :=
    TLFragmenter(beatBytes, lineBytes) := desser.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle { val finished = Output(Bool()) })

    val mergeType = serdes.module.mergeTypes(0)
    val wordsPerBeat = (mergeType.getWidth - 1) / serWidth + 1
    val beatsPerBlock = lineBytes / beatBytes
    val qDepth = (wordsPerBeat * beatsPerBlock) << idBits

    desser.module.io.ser.head.in <> Queue(serdes.module.io.ser.head.out, qDepth)
    serdes.module.io.ser.head.in <> Queue(desser.module.io.ser.head.out, qDepth)
    io.finished := fuzzer.module.io.finished
  }
}

class SerdesTestWrapper(implicit p: Parameters) extends UnitTest {
  val testReset = RegInit(true.B)
  val test = Module(LazyModule(new SerdesTest).module)
  io.finished := test.io.finished
  test.reset := testReset

  when (testReset && io.start) { testReset := false.B }
}

class BidirectionalSerdesTest(implicit p: Parameters) extends LazyModule {
  val idBits = 2
  val beatBytes = 8
  val lineBytes = 64
  val serWidth = 32

  val fuzzer = LazyModule(new TLFuzzer(
    nOperations = 32,
    inFlight = 1 << idBits))

  val serdes = LazyModule(new TLSerdesser(
    w = serWidth,
    clientParams = TLClientParameters(
      name = "tl-desser",
      sourceId = IdRange(0, 1 << idBits)),
    managerParams = TLManagerParameters(
      address = Seq(AddressSet(0, 0xffff)),
      regionType = RegionType.UNCACHED,
      supportsGet = TransferSizes(1, lineBytes),
      supportsPutFull = TransferSizes(1, lineBytes)),
    beatBytes = beatBytes))

  val testram = LazyModule(new TLTestRAM(
    address = AddressSet(0, 0xffff),
    beatBytes = beatBytes))

  serdes.managerNode := TLBuffer() := fuzzer.node
  testram.node := TLBuffer() :=
    TLFragmenter(beatBytes, lineBytes) := serdes.clientNode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle { val finished = Output(Bool()) })

    val mergeType = serdes.module.mergeType
    val wordsPerBeat = (mergeType.getWidth - 1) / serWidth + 1
    val beatsPerBlock = lineBytes / beatBytes
    val qDepth = (wordsPerBeat * beatsPerBlock) << idBits

    serdes.module.io.ser.in <> Queue(serdes.module.io.ser.out, qDepth)
    io.finished := fuzzer.module.io.finished
  }
}

class BidirectionalSerdesTestWrapper(implicit p: Parameters) extends UnitTest {
  val testReset = RegInit(true.B)
  val test = Module(LazyModule(new SerdesTest).module)
  io.finished := test.io.finished
  test.reset := testReset

  when (testReset && io.start) { testReset := false.B }
}

class StreamWidthAdapterTest extends UnitTest {
  val smaller = Wire(new StreamIO(16))
  val larger = Wire(new StreamIO(64))

  val data = Vec(
    0xab13.U, 0x71ff.U, 0x6421.U, 0x9123.U,
    0xbbdd.U, 0x1542.U, 0x8912.U)

  val keep = Vec(
    "b11".U, "b10".U, "b11".U, "b00".U,
    "b11".U, "b01".U, "b11".U)

  val (inIdx, inDone)   = Counter(smaller.in.fire(),  data.size)
  val (outIdx, outDone) = Counter(smaller.out.fire(), data.size)

  val started = RegInit(false.B)
  val sending = RegInit(false.B)
  val receiving = RegInit(false.B)

  smaller.out.valid := sending
  smaller.out.bits.data := data(outIdx)
  smaller.out.bits.keep := keep(outIdx)
  smaller.out.bits.last := outIdx === (data.size - 1).U
  smaller.in.ready := receiving

  StreamWidthAdapter(larger, smaller)
  larger.in <> Queue(larger.out, 2)

  when (io.start && !started) {
    started := true.B
    sending := true.B
    receiving := true.B
  }

  when (outDone)  { sending   := false.B }
  when (inDone) { receiving := false.B }

  io.finished := !sending && !receiving

  assert(!smaller.in.valid ||
    (smaller.in.bits.data === data(inIdx) &&
     smaller.in.bits.keep === keep(inIdx) &&
     smaller.in.bits.last === inDone),
    "StreamWidthAdapterTest: Data, keep, or last does not match")
}

class SwitcherDummy(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode("dummy", IdRange(0, 1))

  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.out(0)

    tl.a.valid := false.B
    tl.a.bits  := DontCare
    tl.b.ready := false.B
    tl.c.valid := false.B
    tl.c.bits  := DontCare
    tl.d.ready := false.B
    tl.e.valid := false.B
    tl.e.bits  := DontCare
  }
}

class SwitcherTest(implicit p: Parameters) extends LazyModule {
  val inIdBits = 3
  val beatBytes = 8
  val lineBytes = 64
  val inChannels = 4
  val outChannels = 2
  val outIdBits = inIdBits + log2Ceil(inChannels)
  val address = AddressSet(0x0, 0xffff)

  val fuzzers = Seq.fill(outChannels) {
    LazyModule(new TLFuzzer(
      nOperations = 32,
      inFlight = 1 << inIdBits))
  }

  val dummies = Seq.fill(outChannels) {
    Seq.fill(inChannels/outChannels-1) {
      LazyModule(new SwitcherDummy)
    }
  }

  val switcher = LazyModule(new TLSwitcher(
    inChannels, Seq(1, outChannels), Seq(address),
    beatBytes = beatBytes, lineBytes = lineBytes, idBits = outIdBits))

  val error = LazyModule(new TLError(ErrorParams(
    Seq(address), beatBytes, lineBytes), beatBytes))

  val rams = Seq.fill(outChannels) {
    LazyModule(new TLTestRAM(
      address = address,
      beatBytes = beatBytes))
  }

  fuzzers.zip(dummies).foreach { case (fuzzer, dummy) =>
    dummy.foreach(switcher.innode := _.node)
    switcher.innode := fuzzer.node
  }

  error.node := switcher.outnodes(0)
  rams.foreach(
    _.node :=
    TLBuffer() :=
    TLFragmenter(beatBytes, lineBytes) :=
    switcher.outnodes(1))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle with UnitTestIO)

    io.finished := fuzzers.map(_.module.io.finished).reduce(_ && _)
    switcher.module.io.sel := 1.U
  }
}

class SwitchTestWrapper(implicit p: Parameters) extends UnitTest {
  val test = Module(LazyModule(new SwitcherTest).module)
  test.io.start := io.start
  io.finished := test.io.finished
}

object TestChipUnitTests {
  def apply(implicit p: Parameters): Seq[UnitTest] =
    Seq(
      Module(new BlockDeviceTrackerTestWrapper),
      Module(new SerdesTestWrapper),
      Module(new BidirectionalSerdesTestWrapper),
      Module(new SwitchTestWrapper),
      Module(new StreamWidthAdapterTest))
}
