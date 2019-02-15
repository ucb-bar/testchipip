package testchipip

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem.{BaseSubsystem}
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink.{TLTestRAM, TLROM, TLError, ErrorParams}
import freechips.rocketchip.util._
import freechips.rocketchip.unittest._
import SerialAdapter._

class FESVRWidgetTest(implicit p: Parameters) extends LazyModule {
  val idBits = 2
  val beatBytes = 8
  val lineBytes = 64
  val serWidth = 32

  val fesvrWidget = LazyModule(new TLFESVRWidget(beatBytes))

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

  // data to be read from
  val romData = Seq(
    BigInt("7766554433221100", 16),
    BigInt("FFEEDDCCBBAA9988", 16),
    BigInt("0123456789ABCDEF", 16),
    BigInt("FEDCBA9876543210", 16),
    BigInt("76543210FDECBA98", 16))

  // this flatmap is creating an array of bytes where the BigInt from above is split into byte blocks
  val rom = LazyModule(new TLROM(0, 64,
    romData.flatMap(
      data => (0 until 8).map(i => ((data >> (i * 8)) & 0xff).toByte)),
    beatBytes = beatBytes))


  // implementation of the module
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

class FESVRWidgthTestWrapper(implicit p: Parameters) extends UnitTest {
  val testReset = RegInit(true.B)
  val test = Module(LazyModule(new SerdesTest).module)
  io.finished := test.io.finished
  test.reset := testReset

  when (testReset && io.start) { testReset := false.B }
}
