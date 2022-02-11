package testchipip

import scala.util.Random

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem.{BaseSubsystem}
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink.{TLTestRAM, TLROM, TLError}
import freechips.rocketchip.util._
import freechips.rocketchip.unittest._

import SerialAdapter._

/**
 * Unit test that uses the TLTSIHostWidget to interact with a target Serdesser.
 * Currently only tests the backend (bypassing the mmio).
 */
class TSIHostWidgetBackendTest(implicit p: Parameters) extends LazyModule {
  // params matching the configuration for the TSIHost widget
  val systemBeatBytes = 8 // should match with the host beat bytes
  val params = p(PeripheryTSIHostKey).head
  val targetLineBytes = params.serdesParams.managerPortParams.maxTransfer // should match with host manager line sz
                                                                                      // note: this is hardcoded in the default to 64B
  val hostAddrSet = params.serdesParams.managerPortParams.managers(0).address(0) // should match with target manager addr set
  //val targetAddrSet = params.serdesParams.managerPortParams.address(0) // should match with host manager addr set
  val targetNumXacts = params.serdesParams.clientPortParams.clients(0).sourceId.end // should match the host client num Xacts

  // TSIHost widget in host-land connecting to the target Serdes
  val hostTSIHostWidgetBackend = LazyModule(new TLTSIHostBackend(params))

  // lives in target-land (connects to the TSIHost widget in host-land)
  val targetSerdes = LazyModule(new TLSerdesser(
    w = params.offchipSerialIfWidth,
    clientPortParams = params.serdesParams.clientPortParams,
    managerPortParams = params.serdesParams.managerPortParams))

  targetSerdes.managerNode := TLBuffer() := targetSerdes.clientNode

  // ram living in host-land. for the fuzzer to read and write from
  val hostRam = LazyModule(new TLTestRAM(
    address = hostAddrSet,
    beatBytes = systemBeatBytes))

  // connect the host node to the host ram
  hostRam.node := TLFragmenter(systemBeatBytes, targetLineBytes) := TLBuffer() := hostTSIHostWidgetBackend.externalClientNode

  // implementation of the module
  lazy val module = new LazyModuleImp(this) {
    // i/o to connect to the unit test interface
    val io = IO(new Bundle with UnitTestIO)

    // other parameters
    val mergeType = targetSerdes.module.mergeType
    val wordsPerBeat = (mergeType.getWidth - 1) / params.offchipSerialIfWidth + 1
    val beatsPerBlock = targetLineBytes / systemBeatBytes
    val qDepth = (wordsPerBeat * beatsPerBlock) << log2Ceil(targetNumXacts)

    // amount of words to fuzz (bounded by hostAddrSet.max)
    val totalWordsInput = 50
    require(hostAddrSet.max >= totalWordsInput)

    // note: this fuzzer i/o must match the mmio reg width since that must equal the SerialAdapter i/o
    val hostTsiFuzzer = Module(new TSIFuzzer(serialIfWidth = SerialAdapter.SERIAL_TSI_WIDTH,
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
    io.finished := hostTsiFuzzer.io.finished
  }
}

/**
 * Unit test wrapper for the TSIHostWidgetTest.
 * It connects the finished and start signals for the widget.
 */
class TSIHostWidgetBackendTestWrapper(implicit p: Parameters) extends UnitTest(16384) {
  val testParams = p.alterPartial({
    case PeripheryTSIHostKey => List(TSIHostParams().copy(
      offchipSerialIfWidth = 4,
      serdesParams = TSIHostParams().serdesParams.copy(
        managerPortParams = TSIHostParams().serdesParams.managerPortParams.v1copy(
          managers = TSIHostParams().serdesParams.managerPortParams.managers.map { _.v1copy(
            address = Seq(AddressSet(0, BigInt("FFFFFF", 16)))
          ) }
        )
      )
    ))
  })
  val test = Module(LazyModule(new TSIHostWidgetBackendTest()(testParams)).module)
  io.finished := test.io.finished
  test.io.start := io.start
}

/**
 * Unit test that uses the TLTSIHostWidget to interact with a target Serdesser.
 */
class TSIHostWidgetTest(implicit p: Parameters) extends LazyModule {
  // params matching the configuration for the TSIHost widget
  val systemBeatBytes = 8 // should match with the host beat bytes
  val params = p(PeripheryTSIHostKey).head
  val targetLineBytes = params.serdesParams.managerPortParams.maxTransfer // should match with host manager line sz
                                                                                      // note: this is hardcoded in the default to 64B
  val hostAddrSet = params.serdesParams.managerPortParams.managers(0).address(0) // should match with target manager addr set
  //val targetAddrSet = params.serdesParams.managerParams.address(0) // should match with host manager addr set
  val targetNumXacts = params.serdesParams.clientPortParams.clients(0).sourceId.end // should match the host client num Xacts

  // TSIHost widget in host-land connecting to the target Serdes
  val hostTSIHostWidget = LazyModule(new TLTSIHostWidget(systemBeatBytes, params))

  // lives in target-land (connects to the TSIHost widget in host-land)
  val targetSerdes = LazyModule(new TLSerdesser(
    w = params.offchipSerialIfWidth,
    clientPortParams = params.serdesParams.clientPortParams,
    managerPortParams = params.serdesParams.managerPortParams))

  targetSerdes.managerNode := TLBuffer() := targetSerdes.clientNode

  // ram living in host-land. for the fuzzer to read and write from
  val hostRam = LazyModule(new TLTestRAM(
    address = hostAddrSet,
    beatBytes = systemBeatBytes))

  // connect the host node to the host rom
  hostRam.node := TLFragmenter(systemBeatBytes, targetLineBytes) := TLBuffer() := hostTSIHostWidget.externalClientNode

  // amount of words to fuzz (bounded by hostAddrSet.max)
  val totalWordsInput = 50
  require(hostAddrSet.max >= totalWordsInput)
  val (inMMIOWriteSeq, outMMIOReadSeq) = TSIFuzzerGeneratorWriteReadSeq(serialIfWidth = SerialAdapter.SERIAL_TSI_WIDTH,
                                                                        totalWords = totalWordsInput,
                                                                        maxReqWords = 5,
                                                                        baseAddress = BigInt(0x0))

  // convert to write, and read expects with the data to patterns to send to pattern pusher
  val      writePatternSeq = inMMIOWriteSeq.map(WritePattern(params.mmioBaseAddress + TSIHostWidgetCtrlRegs.txQueueOffset,
                                                             2, // 4 bytes
                                                             _))

  val readExpectPatternSeq = outMMIOReadSeq.map(data => ReadExpectPattern(params.mmioBaseAddress + TSIHostWidgetCtrlRegs.rxQueueOffset,
                                                                          2, // 4 bytes
                                                                          data))

  val patternPusher = LazyModule(new TLPatternPusher("write-mmio-pusher", writePatternSeq ++ readExpectPatternSeq))

  // connect the pattern pusher to the MMIO
  hostTSIHostWidget.mmioNode := patternPusher.node

  // create an i/o node to connect to
  val tsiHostIOSink = hostTSIHostWidget.ioNode.makeSink

  // implementation of the module
  lazy val module = new LazyModuleImp(this) {
    // i/o to connect to the unit test interface
    val io = IO(new Bundle with UnitTestIO)

    val tsiHostPortIO = tsiHostIOSink.bundle

    // other parameters
    val mergeType = targetSerdes.module.mergeType
    val wordsPerBeat = (mergeType.getWidth - 1) / params.offchipSerialIfWidth + 1
    val beatsPerBlock = targetLineBytes / systemBeatBytes
    val qDepth = (wordsPerBeat * beatsPerBlock) << log2Ceil(targetNumXacts)

    // Tie the serial_clock to the implicit clock for tests
    tsiHostPortIO.serial_clock := clock

    // connect the output of the host widget serial link to the target serdes serial link
    tsiHostPortIO.serial.in <> Queue(targetSerdes.module.io.ser.out, qDepth)
    targetSerdes.module.io.ser.in <> Queue(tsiHostPortIO.serial.out, qDepth)

    val started = RegInit(false.B)
    when (io.start){
      started := true.B
    }

    // connect unit test signals
    patternPusher.module.io.run := started
    io.finished := patternPusher.module.io.done
  }
}

/**
 * Unit test wrapper for the TSIHostWidgetTest.
 * It connects the finished and start signals for the widget.
 *
 * TODO: FIXME: Since the MMIO enqueue is non-blocking, the PatternPusher can
 * continually write to the txqueue dropping messages sent.
 */
class TSIHostWidgetTestWrapper(implicit p: Parameters) extends UnitTest(16384) {
  val testParams = p.alterPartial({
    case PeripheryTSIHostKey => List(TSIHostParams().copy(
      offchipSerialIfWidth = 4,
      txQueueEntries = 32,
      rxQueueEntries = 32,
      serdesParams = TSIHostParams().serdesParams.copy(
        managerPortParams = TSIHostParams().serdesParams.managerPortParams.v1copy(
          managers = TSIHostParams().serdesParams.managerPortParams.managers.map { _.v1copy(
            address = Seq(AddressSet(0, BigInt("FFFFFF", 16)))
          ) }
        )
      )
    ))
  })
  val test = Module(LazyModule(new TSIHostWidgetTest()(testParams)).module)
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
  val dataWidth = SerialAdapter.SERIAL_TSI_WIDTH
  val dataMask = (BigInt(1) << dataWidth) - 1

  // max split width, aka tsi sends this amount of bits for the address and len
  // note: this is split into dataWidth words
  val maxSplitWidth = 64

  /**
   * Split a 64b entry into a sequence of multiple dataWidth words.
   *
   * @param data data to convert to sequence of words
   * @return seq of two bigints with the data split between them
   */
  private def split(data: BigInt): Seq[BigInt] = {
    // check to make sure things are the right size
    require((if (data == 0) 1 else log2Ceil(data)) <= maxSplitWidth)

    // generate sequence
    Seq(data & dataMask, (data >> dataWidth) & dataMask)
  }

  /**
   * Create a write TSI sequence of words.
   *
   * @param address address to write to
   * @param data sequence of data to write
   * @return seq representing a tsi write sequence
   */
  def Write(address: BigInt, data: Seq[BigInt]): Seq[BigInt] = {
    // checks
    data.map(x => require((if (x == 0) 1 else log2Ceil(x)) <= dataWidth))

    // generate sequence
    Seq(SAI_CMD_WRITE) ++ split(address) ++ split(data.size - 1) ++ data
  }

  /**
   * Create a read TSI sequence of words.
   *
   * @param address address to read from
   * @param len length of words to read
   * @return seq representing a tsi read sequence
   */
  def Read(address: BigInt, len: BigInt): Seq[BigInt] = {
    // generate sequence
    Seq(SAI_CMD_READ) ++ split(address) ++ split(len - 1)
  }
}

/**
 * Factory for write/read TSI sequences
 */
object TSIFuzzerGeneratorWriteReadSeq {
  /**
   * Create a TSI input write/read sequence and the corresponding
   * output expectation sequence of words
   *
   * @param serialIfWidth size of serial interface out of the TSIFuzzer (word size) (currently only supports 32b or lower)
   * @param totalWords total amount of words read/written using TSI requests
   * @param maxReqWords max number of words read/written in a single TSI request
   * @param baseAddress start base address to write/read words
   * @return two sequences. first seq is a read/write sequence of tsi commands. second seq is the result that you expect on the reads
   */
  def apply(serialIfWidth: Int = 32,
            totalWords: Int = 20,
            maxReqWords: Int = 4,
            baseAddress: BigInt = BigInt(0x0)): (Seq[BigInt], Seq[BigInt]) = {

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
      //println(s"Write TSI Req: (${curAddress}, ${data})")
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
      //println(s"Read TSI Req: (${curAddress}, ${reqWords})")
      inTsiReadSeq = inTsiReadSeq ++ TSIHelper.Read(curAddress, reqWords)
      wordCount = wordCount + reqWords
      readSizes = readSizes :+ reqWords
    }

    // generate the output words to check against
    val inputTsiSeq = inTsiWriteSeq ++ inTsiReadSeq
    //println(s"In MMIO Seq: ${inputTsiSeq}")
    var allData = allDataWritten.reverse
    var outputTsiData = Seq[BigInt]()
    for (i <- 0 until readSizes.size) {
      val readSize = readSizes(i)
      outputTsiData = outputTsiData ++ allData.take(readSize).reverse
      allData = allData.drop(readSize)
    }
    //println(s"Out Seq: ${outputTsiData}")

    (inputTsiSeq, outputTsiData)
  }
}

/**
 * Generates a set of R/W TSI requests.
 * All writes occur before reads.
 * Reads will read all the values that were written.
 * Only writes/reads from [baseAddress, baseAddress + (totalWords * serialIfWidth/8))
 *
 * @param serialIfWidth size of serial interface out of the TSIFuzzer (word size) (currently only supports 32b or lower)
 * @param totalWords total amount of words read/written using TSI requests
 * @param maxReqWords max number of words read/written in a single TSI request
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

  val (inputTsiSeq, outputTsiData) = TSIFuzzerGeneratorWriteReadSeq(serialIfWidth, totalWords, maxReqWords, baseAddress)

  val inputTsiSeqVec = VecInit(inputTsiSeq.map(_.U(serialIfWidth.W)))
  val outputTsiDataVec = VecInit(outputTsiData.map(_.U(serialIfWidth.W)))

  // count when the req's are done and when the completion is signaled
  val (inputTsiSeqIdx, inputTsiSeqDone) = Counter(io.serial.out.fire, inputTsiSeqVec.size)
  val (outputTsiDataIdx, outputTsiDataDone) = Counter(io.serial.in.fire, outputTsiDataVec.size)

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
  when (io.serial.out.fire) { printf("In: idx: (%d) data: (0x%x)\n", inputTsiSeqIdx, inputTsiSeqVec(inputTsiSeqIdx)) }
  when (io.serial.in.fire) {
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
