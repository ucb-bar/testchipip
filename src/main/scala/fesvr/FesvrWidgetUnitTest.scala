package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem.{BaseSubsystem}
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink.{TLTestRAM, TLROM, TLError, ErrorParams}
import freechips.rocketchip.util._
import freechips.rocketchip.unittest._

import SerialAdapter._

/**
 * Unit test that uses the TLFESVRWidget to interact with a target Serdesser.
 * Currently only tests read TSI requests since TLROM can be setup easily with data
 * that way.
 */
class FESVRWidgetTest(implicit p: Parameters) extends LazyModule {
  // params matching the configuration for the FESVR widget
  val beatBytes = 8
  val lineBytes = p(PeripheryFESVRKey).tlLineBytes
  val serWidth = p(PeripheryFESVRKey).serialIfWidth
  val addrSet = p(PeripheryFESVRKey).tlAddressSet
  val regionTyp = p(PeripheryFESVRKey).tlRegionType
  val numXacts = p(PeripheryFESVRKey).tlNumTransactions

  // FESVR widget in host-land connecting to the target Serdes
  val hostFESVRWidget = LazyModule(new TLFESVRWidget(beatBytes))

  // lives in target-land (connects to the FESVR widget in host-land)
  val targetSerdes = LazyModule(new TLSerdesser(
    w = serWidth,
    clientParams = TLClientParameters(
      name = "tl-desser",
      sourceId = IdRange(0, numXacts)),
    managerParams = TLManagerParameters(
      address = Seq(addrSet),
      regionType = regionTyp,
      supportsGet = TransferSizes(1, lineBytes),
      supportsPutFull = TransferSizes(1, lineBytes),
      supportsPutPartial = TransferSizes(1, lineBytes)),
    beatBytes = beatBytes))

  // rom living in target-land. connects to the client of the serdes
  val targetRomData = Seq(
    BigInt("7766554433221100", 16),
    BigInt("FFEEDDCCBBAA9988", 16),
    BigInt("0123456789ABCDEF", 16),
    BigInt("FEDCBA9876543210", 16),
    BigInt("76543210FDECBA98", 16))
  val targetRom = LazyModule(new TLROM(0, 64,
    targetRomData.flatMap(
      data => (0 until 8).map(i => ((data >> (i * 8)) & 0xff).toByte)),
    beatBytes = beatBytes))

  // TLFragmenter splits up the multi beat packet into single beats that the left hand side can take
  // (of size beatBytes).
  targetRom.node := TLFragmenter(beatBytes, lineBytes) := TLBuffer() := targetSerdes.clientNode

  // fuzzer that lives in target-land that sends random requests to the ram on the host
  val targetFuzzer = LazyModule(new TLFuzzer(
    nOperations = 32,
    inFlight = numXacts))

  // ram living in host-land. for the fuzzer to read and write from
  val hostRam = LazyModule(new TLTestRAM(
    address = addrSet,
    beatBytes = beatBytes))

  // connect fuzzer to the target serdes
  targetSerdes.managerNode := TLBuffer() := targetFuzzer.node
  // connect the host node to the host rom
  hostRam.node := TLFragmenter(beatBytes, lineBytes) := TLBuffer() := hostFESVRWidget.externalClientNode

  // implementation of the module
  lazy val module = new LazyModuleImp(this) {
    // i/o to connect to the unit test interface
    val io = IO(new Bundle with UnitTestIO)

    // TSI cmd of the form (cmd, lowAddr, highAddr, lowLen, highLen)
    // note that len is actually the len - 1 (e.g. len of 4 is really 5)
    // and len is in increments of 32b
    // O is read, 1 is write
    val inputTsiSeq = Seq(Seq(0,  0x0, 0, 0x0, 0),
                          Seq(0,  0x4, 0, 0x0, 0),
                          Seq(0,  0xC, 0, 0x0, 0),
                          Seq(0, 0x10, 0, 0x0, 0),
                          Seq(0, 0x20, 0, 0x1, 0))

    // 32b sequences expected from the TSI response
    val outputTsiData = Seq(Seq(BigInt("33221100", 16)),
                            Seq(BigInt("77665544", 16)),
                            Seq(BigInt("FFEEDDCC", 16)),
                            Seq(BigInt("89ABCDEF", 16)),
                            Seq(BigInt("FDECBA98", 16), BigInt("76543210", 16)))

    val inputTsiSeqVec = VecInit(inputTsiSeq.flatMap(x => x).map(_.U((p(PeripheryFESVRKey).mmioRegWidth).W)))
    val outputTsiDataVec = VecInit(outputTsiData.flatMap(x => x).map(_.U((p(PeripheryFESVRKey).mmioRegWidth).W)))

    // other parameters
    val mergeType = targetSerdes.module.mergeType
    val wordsPerBeat = (mergeType.getWidth - 1) / serWidth + 1
    val beatsPerBlock = lineBytes / beatBytes
    val qDepth = (wordsPerBeat * beatsPerBlock) << log2Ceil(numXacts)

    // count when the req's are done and when the completion is signaled
    val (inputTsiSeqIdx, inputTsiSeqDone) = Counter(hostFESVRWidget.module.io.debug.in.fire(), inputTsiSeqVec.size)
    val (outputTsiDataIdx, outputTsiDataDone) = Counter(hostFESVRWidget.module.io.debug.out.fire(), outputTsiDataVec.size)

    val started = RegInit(false.B)
    val sendingTsi = RegInit(false.B)
    val recvingTsi = RegInit(false.B)

    // start unit test
    when (!started && io.start) {
      started := true.B
      sendingTsi := true.B
      recvingTsi := true.B
    }

    // stop when the counters for sending and recieving have saturated
    when (inputTsiSeqDone)  {
      sendingTsi := false.B
    }
    when (outputTsiDataDone) {
      recvingTsi := false.B
    }

    // route the control TSI commands to the debug port on the adapter (bypasses the MMIO)
    hostFESVRWidget.module.io.debug.in.valid := sendingTsi
    hostFESVRWidget.module.io.debug.in.bits := inputTsiSeqVec(inputTsiSeqIdx)

    // output data should match
    hostFESVRWidget.module.io.debug.out.ready := recvingTsi
    assert(!hostFESVRWidget.module.io.debug.out.valid ||
           hostFESVRWidget.module.io.debug.out.bits === outputTsiDataVec(outputTsiDataIdx),
           "TSI Output does not match expected output")

    // connect the output of the host widget serial link to the target serdes serial link
    hostFESVRWidget.module.io.serial.in <> Queue(targetSerdes.module.io.ser.out, qDepth)
    targetSerdes.module.io.ser.in <> Queue(hostFESVRWidget.module.io.serial.out, qDepth)

    // send finished signal
    io.finished := !recvingTsi && !sendingTsi && started && targetFuzzer.module.io.finished

    // debug printfs
    //when ( hostFESVRWidget.module.io.debug.in.fire() ) { printf("In: idx: (%d) data: (0x%x)\n", inputTsiSeqIdx, inputTsiSeqVec(inputTsiSeqIdx)) }
    //when ( hostFESVRWidget.module.io.debug.out.fire() ) {
    //  printf("Out Want: idx: (%d) data: (0x%x)\n", outputTsiDataIdx, outputTsiDataVec(outputTsiDataIdx))
    //  printf("Out  Got: data: (0x%x)\n", hostFESVRWidget.module.io.debug.out.bits)
    //}
  }
}

/**
 * Unit test wrapper for the FESVRWidgetTest.
 * It connects the finished and start signals for the widget.
 */
class FESVRWidgetTestWrapper(implicit p: Parameters) extends UnitTest {
  val testParams = p.alterPartial({
    case PeripheryFESVRKey => FESVRParams(bypassMMIO = true,
                                          tlAddressSet = AddressSet(0, BigInt("3F", 16)))
  })
  val test = Module(LazyModule(new FESVRWidgetTest()(testParams)).module)
  io.finished := test.io.finished
  test.io.start := io.start
}
