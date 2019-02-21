package testchipip

import scala.util.Random

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
 * Unit test that uses the TLTSIHostWidget to interact with a target Serdesser.
 * Currently only tests read TSI requests since TLROM can be setup easily with data
 * that way.
 */
class TSIHostWidgetBackendTest(implicit p: Parameters) extends LazyModule {
  // params matching the configuration for the TSIHost widget
  val systemBeatBytes = 8 // should match with the host beat bytes
  val targetLineBytes = p(PeripheryTSIHostKey).serdesParams.managerParams.maxTransfer // should match with host manager line sz
                                                                                      // note: this is hardcoded in the default to 64B
  val hostAddrSet = p(PeripheryTSIHostKey).serdesParams.managerParams.address(0) // should match with target manager addr set
  val targetAddrSet = p(PeripheryTSIHostKey).serdesParams.managerParams.address(0) // should match with host manager addr set
  val targetNumXacts = p(PeripheryTSIHostKey).serdesParams.clientParams.sourceId.end // should match the host client num Xacts

  // TSIHost widget in host-land connecting to the target Serdes
  val hostTSIHostWidgetBackend = LazyModule(new TLTSIHostBackend(systemBeatBytes))

  // lives in target-land (connects to the TSIHost widget in host-land)
  val targetSerdes = LazyModule(new TLSerdesser(
    w = p(PeripheryTSIHostKey).serialIfWidth,
    clientParams = p(PeripheryTSIHostKey).serdesParams.clientParams,
    managerParams = p(PeripheryTSIHostKey).serdesParams.managerParams,
    beatBytes = systemBeatBytes))

  // ram living in target-land. connects to the client of the serdes
  val targetRam = LazyModule(new TLTestRAM(
    address = targetAddrSet,
    beatBytes = systemBeatBytes))

  // TLFragmenter splits up the multi beat packet into single beats that the left hand side can take
  // (of size systemBeatBytes).
  targetRam.node := TLFragmenter(systemBeatBytes, targetLineBytes) := TLBuffer() := targetSerdes.clientNode

  // fuzzer that lives in target-land that sends random requests to the ram on the host
  val targetFuzzer = LazyModule(new TLFuzzer(
    nOperations = 32,
    inFlight = targetNumXacts))

  // ram living in host-land. for the fuzzer to read and write from
  val hostRam = LazyModule(new TLTestRAM(
    address = hostAddrSet,
    beatBytes = systemBeatBytes))

  // connect fuzzer to the target serdes
  targetSerdes.managerNode := TLBuffer() := targetFuzzer.node
  // connect the host node to the host rom
  hostRam.node := TLFragmenter(systemBeatBytes, targetLineBytes) := TLBuffer() := hostTSIHostWidgetBackend.externalClientNode

  // implementation of the module
  lazy val module = new LazyModuleImp(this) {
    // i/o to connect to the unit test interface
    val io = IO(new Bundle with UnitTestIO)

    // other parameters
    val mergeType = targetSerdes.module.mergeType
    val wordsPerBeat = (mergeType.getWidth - 1) / p(PeripheryTSIHostKey).serialIfWidth + 1
    val beatsPerBlock = targetLineBytes / systemBeatBytes
    val qDepth = (wordsPerBeat * beatsPerBlock) << log2Ceil(targetNumXacts)

    // amount of words to fuzz (bounded by targetAddrSet.max)
    val totalWordsInput = 100
    require(targetAddrSet.max >= totalWordsInput)

    val hostTsiFuzzer = Module(new TSIFuzzer(serialIfWidth = p(PeripheryTSIHostKey).serialIfWidth,
                                         totalWords = totalWordsInput,
                                         maxReqWords = 5,
                                         baseAddress = BigInt(0x0)))

    // route the control TSI data to/from the backend (bypasses the MMIO frontend)
    hostTSIHostWidgetBackend.module.io.adapterSerial.in <> hostTsiFuzzer.io.serial.out
    hostTsiFuzzer.io.serial.in <> hostTSIHostWidgetBackend.module.io.adapterSerial.out

    // connect the output of the host widget serial link to the target serdes serial link
    hostTSIHostWidgetBackend.module.io.serdesSerial.in <> Queue(targetSerdes.module.io.ser.out, qDepth)
    targetSerdes.module.io.ser.in <> Queue(hostTSIHostWidgetBackend.module.io.serdesSerial.out, qDepth)

    // connect unit test signals
    hostTsiFuzzer.io.start := io.start
    io.finished := hostTsiFuzzer.io.finished && targetFuzzer.module.io.finished
  }
}

/**
 * Unit test wrapper for the TSIHostWidgetTest.
 * It connects the finished and start signals for the widget.
 */
class TSIHostWidgetBackendTestWrapper(implicit p: Parameters) extends UnitTest {
  val testParams = p.alterPartial({
    case PeripheryTSIHostKey => TSIHostParams().copy(
      serdesParams = TSIHostParams().serdesParams.copy(
        managerParams = TSIHostParams().serdesParams.managerParams.copy(
          address = Seq(AddressSet(0, BigInt("FFFFFF", 16)))
        )
      )
    )
  })
  val test = Module(LazyModule(new TSIHostWidgetBackendTest()(testParams)).module)
  io.finished := test.io.finished
  test.io.start := io.start
}

/**
 * Factory that creates a TSI Seq of words to be used in testing
 */
object TSIHelper {
  // default values for TSI read/write
  val SAI_CMD_WRITE = BigInt(1)
  val SAI_CMD_READ = BigInt(0)

  // currently only supports 32b for the word size
  val serialIfWidth = 32

  /**
   * Split a 64b entry into a sequence of multiple serialIfWidth words/
   *
   * @param data data to convert to sequence of words
   */
  private def split(data: BigInt): Seq[BigInt] = {
    val mask = 0xFFFFFFFF
    Seq(data & mask, (data >> serialIfWidth) & mask)
  }

  /**
   * Create a write TSI sequence of words.
   *
   * @param address address to write to
   * @param data sequence of data to write
   */
  def Write(address: BigInt, data: Seq[BigInt]): Seq[BigInt] = {
    // checks
    require((if (address == 0) 1 else log2Ceil(address)) <= 64)
    data.map(x => require((if (x == 0) 1 else log2Ceil(x)) <= serialIfWidth))

    // generate sequence
    Seq(SAI_CMD_WRITE) ++ split(address) ++ split(data.size - 1) ++ data
  }

  /**
   * Create a read TSI sequence of words.
   *
   * @param address address to read from
   * @param len length of words to read
   */
  def Read(address: BigInt, len: BigInt): Seq[BigInt] = {
    // checks
    require((if (address == 0) 1 else log2Ceil(address)) <= 64)

    // generate sequence
    Seq(SAI_CMD_READ) ++ split(address) ++ split(len - 1)
  }
}

/**
 * Generates a set of R/W TSI requests.
 * All writes occur before reads.
 * Reads will read all the values that were written.
 * Only writes/reads from [baseAddress, baseAddress + (maxReqWords * serialIfWidth/8))
 *
 * @param serialIfWidth size of serial interface (word size) (currently only supports 32b or lower)
 * @param totalWords total amount of words read/written using TSI requests
 * @param maxReqWords max number of words read/written in a TSI request
 * @param baseAddress start base address to write/read words
 */
class TSIFuzzer(val serialIfWidth: Int = 32,
                val totalWords: Int = 20,
                val maxReqWords: Int = 4,
                val baseAddress: BigInt = BigInt(0x0)) extends Module {
  val io = IO(new Bundle {
    val serial = new SerialIO(serialIfWidth)
    val start = Input(Bool())
    val finished = Output(Bool())
  })

  val rand = new Random()
  val serialIfWidthBytes = serialIfWidth / 8

  var inTsiWriteSeq = Seq[BigInt]()
  var allDataWritten = Seq[BigInt]()
  var readSizes = Seq[Int]()
  var inTsiReadSeq = Seq[BigInt]()

  // generate write req's
  var curAddress = baseAddress
  var wordCount = 0 // running count of amount of bytes written
  while (wordCount < totalWords) {
    // get a valid random number of words to write
    var reqWords = 0
    do {
      reqWords = rand.nextInt(maxReqWords) + 1
    } while (reqWords + wordCount > totalWords)

    // generate the random data of size reqWords
    var data = Seq[BigInt]()
    for (i <- 0 until reqWords) {
      data = data :+ BigInt(serialIfWidth, rand)
    }

    // generate write req
    println(s"Write TSI Req: (${curAddress}, ${data})")
    inTsiWriteSeq = inTsiWriteSeq ++ TSIHelper.Write(curAddress, data)
    curAddress = curAddress + (serialIfWidthBytes * reqWords)
    allDataWritten = allDataWritten ++ data
    wordCount = wordCount + reqWords
  }

  // generate read req's (read backwards)
  wordCount = 0
  curAddress = baseAddress + (serialIfWidthBytes * totalWords)
  while (wordCount < totalWords) {
    // get a valid random number of words to read
    var reqWords = 0
    do {
      reqWords = rand.nextInt(maxReqWords) + 1
    } while (reqWords + wordCount > totalWords)

    // generate read req
    curAddress = curAddress - (serialIfWidthBytes * reqWords)
    println(s"Read TSI Req: (${curAddress}, ${reqWords})")
    inTsiReadSeq = inTsiReadSeq ++ TSIHelper.Read(curAddress, reqWords)
    wordCount = wordCount + reqWords
    readSizes = readSizes :+ reqWords
  }

  // generate the output words to check against
  val inputTsiSeq = inTsiWriteSeq ++ inTsiReadSeq
  println(s"In Seq: ${inputTsiSeq}")
  var allData = allDataWritten.reverse
  var outputTsiData = Seq[BigInt]()
  for (i <- 0 until readSizes.size) {
    val readSize = readSizes(i)
    outputTsiData = outputTsiData ++ allData.take(readSize).reverse
    allData = allData.drop(readSize)
  }
  println(s"Out Seq: ${outputTsiData}")

  val inputTsiSeqVec = VecInit(inputTsiSeq.map(_.U(serialIfWidth.W)))
  val outputTsiDataVec = VecInit(outputTsiData.map(_.U(serialIfWidth.W)))

  // count when the req's are done and when the completion is signaled
  val (inputTsiSeqIdx, inputTsiSeqDone) = Counter(io.serial.out.fire(), inputTsiSeqVec.size)
  val (outputTsiDataIdx, outputTsiDataDone) = Counter(io.serial.in.fire(), outputTsiDataVec.size)

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

  // route the control TSI commands out
  io.serial.out.valid := sendingTsi
  io.serial.out.bits := inputTsiSeqVec(inputTsiSeqIdx)

  // debug printfs
  //when (io.serial.out.fire()) { printf("In: idx: (%d) data: (0x%x)\n", inputTsiSeqIdx, inputTsiSeqVec(inputTsiSeqIdx)) }
  when (io.serial.in.fire()) {
    printf("Out Want: idx: (%d) data: (0x%x)\n", outputTsiDataIdx, outputTsiDataVec(outputTsiDataIdx))
    printf("Out  Got: data: (0x%x)\n", io.serial.in.bits)
  }

  // output data should match
  io.serial.in.ready := recvingTsi
  assert(!io.serial.in.valid ||
         io.serial.in.bits === outputTsiDataVec(outputTsiDataIdx),
         "TSI output data does not match expected output")

  // send finished signal
  io.finished := !recvingTsi && !sendingTsi && started
}
