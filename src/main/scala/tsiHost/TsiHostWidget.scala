package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem.{BaseSubsystem}
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import SerialAdapter._

/**
 * Parameters for the SerDes
 *
 * @param clientParams client parameters that receives the SerialIO inwards
 * @param managerParams manager parameters that sends the SerialIO outwards
 */
case class TSIHostSerdesParams(
  clientParams: TLClientParameters = TLClientParameters(
    name = "tl-tsi-host-serdes",
    sourceId = IdRange(0, 1)),
  managerParams: TLManagerParameters = TLManagerParameters(
    address = Seq(AddressSet(0, BigInt("FFFFFFFF", 16))),
    regionType = RegionType.UNCACHED,
    supportsGet = TransferSizes(1, 64),
    supportsPutFull = TransferSizes(1, 64),
    supportsPutPartial = TransferSizes(1, 64))
)


/**
 * TSI Host parameter class
 *
 * @param mmioRegWidth size of the MMIO data being sent back and forth in bits
 * @param serialIfWidth size of the serialIO *out* of the widget
 * @param txQueueEntries size of the queue for sending TSI requests
 * @param rxQueueEntries size of the queue for receiving TSI responses
 * @param baseAddress start address of the MMIO registers
 */
case class TSIHostParams(
  mmioRegWidth: Int = 32,
  serialIfWidth: Int = 32,
  txQueueEntries: Int = 16,
  rxQueueEntries: Int = 16,
  baseAddress: BigInt = BigInt(0x10017000),
  serdesParams: TSIHostSerdesParams = TSIHostSerdesParams()
)

/**
 * Offsets for the MMIO communication queues (base + offset to get the proper address)
 */
object TSIHostWidgetCtrlRegs {
  val txQueueOffset = 0x00
  val rxQueueOffset = 0x04
}

/**
 * I/O to the outside world. This is the stream data out of the TSIHostWidget.
 *
 * @param w width in bits of connection to outside world
 */
class TSIHostWidgetIO(val w: Int)(implicit val p: Parameters) extends Bundle {
  val serial = new SerialIO(w)
}

/**
 * I/O bundle to connect to the MMIO frontend class
 */
trait TLTSIHostMMIOFrontendBundle {
  implicit val p: Parameters
  val serial = new SerialIO(p(PeripheryTSIHostKey).mmioRegWidth)
}

/**
 * Mixin defining the module used to communicate between the MMIO and the parser
 */
trait TLTSIHostMMIOFrontendModule extends HasRegMap {
  implicit val p: Parameters

  val io: TLTSIHostMMIOFrontendBundle

  val txQueue = Module(new Queue(UInt(p(PeripheryTSIHostKey).mmioRegWidth.W), p(PeripheryTSIHostKey).txQueueEntries)) // where is the queue being dequeued (to the SerialAdapter)
  val rxQueue = Module(new Queue(UInt(p(PeripheryTSIHostKey).mmioRegWidth.W), p(PeripheryTSIHostKey).rxQueueEntries)) // where is the queue being enqueued (from the SerialAdapter)

  io.serial.out <> txQueue.io.deq
  rxQueue.io.enq <> io.serial.in

  // memory mapped registers and connections to the queues
  regmap(
    TSIHostWidgetCtrlRegs.txQueueOffset -> Seq(RegField.w(p(PeripheryTSIHostKey).mmioRegWidth, txQueue.io.enq)),
    TSIHostWidgetCtrlRegs.rxQueueOffset -> Seq(RegField.r(p(PeripheryTSIHostKey).mmioRegWidth, rxQueue.io.deq))
  )
}

/**
 * Top level class that uses a register router to connect MMIO to the core. This instantiates
 * the implementation module and bundle
 *
 * @param beatBytes amount of bytes to send per beat
 */
class TLTSIHostMMIOFrontend(val beatBytesIn: Int)(implicit p: Parameters)
  extends TLRegisterRouter(
    base = p(PeripheryTSIHostKey).baseAddress,
    devname = "tsi-host-mmio-frontend",
    devcompat = Seq("ucbbar,tsi-host"),
    beatBytes = beatBytesIn)(
      new TLRegBundle(p(PeripheryTSIHostKey), _)    with TLTSIHostMMIOFrontendBundle)(
      new TLRegModule(p(PeripheryTSIHostKey), _, _) with TLTSIHostMMIOFrontendModule)

/**
 * Module to decouple the SerialAdapter and TLSerdesser from the Frontend
 * connection type (MMIO, other...)
 *
 * @param beatBytesIn amount of bytes to send per beat
 */
class TLTSIHostBackend(val beatBytesIn: Int)(implicit p: Parameters)
  extends LazyModule
{
   // module to take in a decoupled io tsi stream and convert to a TL stream
  val serialAdapter = LazyModule(new SerialAdapter)
  // This converts the TL signals given by the parser into a decoupled stream
  val serdes = LazyModule(new TLSerdesser(
        w = p(PeripheryTSIHostKey).serialIfWidth,
        clientParams = p(PeripheryTSIHostKey).serdesParams.clientParams,
        managerParams = p(PeripheryTSIHostKey).serdesParams.managerParams,
        beatBytes = beatBytesIn))

  // currently the amount of data out of the mmio regs should equal the serial IO
  require(p(PeripheryTSIHostKey).serialIfWidth == SERIAL_IF_WIDTH)

  // create TL node to connect to outer bus
  val externalClientNode = TLIdentityNode()

  // you are sending the TL request outwards... to the serdes manager... then to a serial stream
  serdes.managerNode := TLBuffer() := serialAdapter.node
  // send TL transaction to the memory system on this side
  externalClientNode := serdes.clientNode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val adapterSerial = new SerialIO(p(PeripheryTSIHostKey).serialIfWidth)
      val serdesSerial = new SerialIO(p(PeripheryTSIHostKey).serialIfWidth)
    })

    val adapterMod = serialAdapter.module
    val serdesMod = serdes.module

    // connect MMIO to the parser
    io.adapterSerial.out <> adapterMod.io.serial.out
    adapterMod.io.serial.in <> io.adapterSerial.in

    // connect to the outside world
    serdesMod.io.ser.in <> io.serdesSerial.in // input decoupled to start serializing (from the target)
    io.serdesSerial.out <> serdesMod.io.ser.out // output of the SerialAdapter (sends a serialized stream to the target)
  }
}

/**
 * TSIHostWidget to connect the Front End SerVeR which sends TSI to a target TL module
 * that resides across a SerialIO boundary.
 *
 * TX is MMIO -> Queue -> SerialAdapter -> TL -> TLSerdesser Out
 * RX is TLSerdesser In -> SerialAdapter -> Queue -> MMIO
 *
 * @param beatBytes amount of bytes to send per beat
 */
class TLTSIHostWidget(val beatBytes: Int)(implicit p: Parameters)
  extends LazyModule
{
  // this should communicate over MMIO to the core (tx and rx)
  val mmioFrontend = LazyModule(new TLTSIHostMMIOFrontend(beatBytes))

  // should convert the communication to/from TL (aka TL -> Parse -> MMIO, MMIO -> Parse -> TL)
  val backend = LazyModule(new TLTSIHostBackend(beatBytes))

  // currently the amount of data out of the MMIO regs should equal the SerialIO
  require(p(PeripheryTSIHostKey).serialIfWidth == SERIAL_IF_WIDTH)

  // create TL node for MMIO
  val mmioNode = TLIdentityNode()
  // create TL node to connect to outer bus
  val externalClientNode = TLIdentityNode()

  // setup the TL connection graph
  mmioFrontend.node := TLAtomicAutomata() := mmioNode
  // send TL transaction to the memory system on the host
  externalClientNode := TLBuffer() := backend.externalClientNode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new TSIHostWidgetIO(p(PeripheryTSIHostKey).serialIfWidth))

    val backendMod = backend.module
    val mmioMod = mmioFrontend.module

    // connect MMIO to the backend
    mmioMod.io.serial.in <> Queue(backendMod.io.adapterSerial.out)
    backendMod.io.adapterSerial.in <> Queue(mmioMod.io.serial.out)

    // connect to the outside world
    backendMod.io.serdesSerial.in <> io.serial.in // input decoupled to start serializing (comes from the target)
    io.serial.out <> backendMod.io.serdesSerial.out // output of the SerialAdapter (sends a serialized stream to the target world)
  }
}
