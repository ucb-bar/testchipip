package testchipip

import chisel3._
import chisel3.util._
import chisel3.core.{withClockAndReset}

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
    sourceId = IdRange(0, 2)),
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
 * @param mmioRegWidth size of the MMIO data being sent back and forth in bits (connects to SerialAdapter)
 * @param serialIfWidth size of the serialIO *out* of the widget (connects TLSerdessers)
 * @param txQueueEntries size of the queue for sending TSI requests
 * @param rxQueueEntries size of the queue for receiving TSI responses
 * @param mmioBaseAddress start address of the MMIO registers
 */
case class TSIHostParams(
  mmioRegWidth: Int = 32,
  serialIfWidth: Int = 32,
  txQueueEntries: Int = 16,
  rxQueueEntries: Int = 16,
  mmioBaseAddress: BigInt = BigInt("10017000", 16),
  targetBaseAddress: BigInt = BigInt("80000000", 16),
  serdesParams: TSIHostSerdesParams = TSIHostSerdesParams()
)

/**
 * Offsets for the MMIO communication queues (base + offset to get the proper address)
 */
object TSIHostWidgetCtrlRegs {
  val txQueueOffset = 0x00 // note: these assume mmioRegWidth = 32b
  val rxQueueOffset = 0x04
  val queueStatusesOffset = 0x08
}

/**
 * I/O to the outside world. This is the stream data out of the TSIHostWidget and
 * the clock for the widget.
 *
 * @param w width in bits of serial TL connection to outside world
 */
class TSIHostWidgetIO(val w: Int) extends Bundle {
  val serial = new SerialIO(w)
  val serial_clock = Input(new Clock())
}

/**
 * I/O bundle to connect to the MMIO frontend class
 */
trait TLTSIHostMMIOFrontendBundle {
  implicit val p: Parameters

  val params: TSIHostParams

  val serial = new SerialIO(params.mmioRegWidth)
}

/**
 * Mixin defining the module used to communicate between the MMIO and the TSI to TL converter
 */
trait TLTSIHostMMIOFrontendModule extends HasRegMap {
  implicit val p: Parameters

  val io: TLTSIHostMMIOFrontendBundle

  val params: TSIHostParams

  // tsi format input/output queues
  val txQueue = Module(new Queue(UInt(params.mmioRegWidth.W), params.txQueueEntries)) // where is the queue being dequeued (to the SerialAdapter)
  val rxQueue = Module(new Queue(UInt(params.mmioRegWidth.W), params.rxQueueEntries)) // where is the queue being enqueued (from the SerialAdapter)

  // status indicators
  val txQueueFull = !txQueue.io.enq.ready
  val rxQueueEmpty = !rxQueue.io.deq.valid

  // connect queues to the backend
  io.serial.out <> txQueue.io.deq
  rxQueue.io.enq <> io.serial.in

  // memory mapped registers and connections to the queues
  regmap(
    TSIHostWidgetCtrlRegs.txQueueOffset -> Seq(RegField.w(params.mmioRegWidth, txQueue.io.enq)),
    TSIHostWidgetCtrlRegs.rxQueueOffset -> Seq(RegField.r(params.mmioRegWidth, rxQueue.io.deq)),
    TSIHostWidgetCtrlRegs.queueStatusesOffset -> Seq(
      RegField.r(8, txQueueFull),
      RegField.r(8, rxQueueEmpty)
    )
  )
}

/**
 * Top level class that uses a register router to connect MMIO to the core. This instantiates
 * the implementation module and bundle
 *
 * @param beatBytes amount of bytes to send per beat
 * @param params the TSI parameters for the widget
 */
class TLTSIHostMMIOFrontend(val beatBytesIn: Int, params: TSIHostParams)(implicit p: Parameters)
  extends TLRegisterRouter(
    base = params.mmioBaseAddress,
    devname = "tsi-host-mmio-frontend",
    devcompat = Seq("ucbbar,tsi-host"),
    beatBytes = beatBytesIn)(
      new TLRegBundle(params, _)    with TLTSIHostMMIOFrontendBundle)(
      new TLRegModule(params, _, _) with TLTSIHostMMIOFrontendModule)

/**
 * Module to decouple the SerialAdapter and TLSerdesser from the Frontend
 * connection type (MMIO, other...)
 *
 * @param beatBytesIn amount of bytes to send per beat
 * @param params the TSI parameters for the widget
 */
class TLTSIHostBackend(val beatBytesIn: Int, val params: TSIHostParams)(implicit p: Parameters)
  extends LazyModule
{
   // module to take in a decoupled io tsi stream and convert to a TL stream
  val serialAdapter = LazyModule(new SerialAdapter)
  // This converts the TL signals given by the serial adapter into a decoupled stream
  val serdes = LazyModule(new TLSerdesser(
        w = params.serialIfWidth,
        clientParams =  params.serdesParams.clientParams,
        managerParams = params.serdesParams.managerParams,
        beatBytes = beatBytesIn,
        onTarget = false))

  // currently the amount of data out of the mmio regs should equal the serial IO
  require(params.mmioRegWidth == SERIAL_IF_WIDTH)

  // create TL node to connect to outer bus
  val externalClientNode = TLIdentityNode()

  // you are sending the TL request outwards... to the serdes manager... then to a serial stream
  serdes.managerNode := TLBuffer() := serialAdapter.node
  // send TL transaction to the memory system on this side
  externalClientNode := serdes.clientNode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val adapterSerial = new SerialIO(params.mmioRegWidth)
      val serdesSerial =  new SerialIO(params.serialIfWidth)
    })

    val adapterMod = serialAdapter.module
    val serdesMod = serdes.module

    // connect MMIO to the encoder/decoder
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
 * @param params the TSI parameters for the widget
 */
class TLTSIHostWidget(val beatBytes: Int, val params: TSIHostParams)(implicit p: Parameters)
  extends LazyModule
{
  // this should communicate over MMIO to the core (tx and rx)
  val mmioFrontend = LazyModule(new TLTSIHostMMIOFrontend(beatBytes, params))

  // should convert the communication to/from TL (i.e. TL -> Encode -> MMIO, MMIO -> Decode -> TL)
  val backend = LazyModule(new TLTSIHostBackend(beatBytes, params))

  // create TL node for MMIO
  val mmioNode = TLIdentityNode()
  // create TL node to connect to outer bus
  val externalClientNode = TLIdentityNode()

  // set up async fifos
  val mmioSink = LazyModule(new TLAsyncCrossingSink)
  val clientSource = LazyModule(new TLAsyncCrossingSource)

  // setup the TL connection graph
  (mmioFrontend.node
    := mmioSink.node
    := TLAsyncCrossingSource()
    := TLAtomicAutomata()
    := mmioNode)
  // send TL transaction to the memory system on the host
  (externalClientNode
    := TLAsyncCrossingSink()
    := clientSource.node
    := backend.externalClientNode)

  // io node handle to create source and sink io's
  val ioNode = BundleBridgeSource(() => new TSIHostWidgetIO(params.serialIfWidth))

  lazy val module = new LazyModuleImp(this) {
    val io = ioNode.bundle

    val backendMod = backend.module
    val mmioMod = mmioFrontend.module
    val mmioSinkMod = mmioSink.module
    val clientSourceMod = clientSource.module

    val syncReset = ResetCatchAndSync(io.serial_clock, reset.toBool)

    // connect other modules to the different clock domain
    Seq(backendMod, mmioMod, mmioSinkMod, clientSourceMod).foreach { m =>
      m.clock := io.serial_clock
      m.reset := syncReset
    }

    withClockAndReset(io.serial_clock, syncReset) {
      // connect MMIO to the backend
      mmioMod.io.serial.in <> Queue(backendMod.io.adapterSerial.out)
      backendMod.io.adapterSerial.in <> Queue(mmioMod.io.serial.out)

      // connect to the outside world
      backendMod.io.serdesSerial.in <> io.serial.in // input decoupled to start serializing (comes from the target)
      io.serial.out <> backendMod.io.serdesSerial.out // output of the SerialAdapter (sends a serialized stream to the target world)
    }
  }
}

/**
 * Parameters to connect a TSI Host Widget to a system
 *
 * @param tsiHostParams the base params for the widget
 * @param controlBus the bus that will be the client for the mmio
 */
case class TSIHostWidgetAttachParams(
  tsiHostParams: TSIHostParams,
  controlBus: TLBusWrapper)(implicit val p: Parameters)

/**
 * Factory object to create TSI Host Widgets
 */
object TLTSIHostWidget {
  /**
   * Just create a TSI widget and connect it to the specified bus
   *
   * @param attachParams params to connect the widget to the mmio bus and instantiate it
   * @return a TSI Host Widget and a TL node to connect to backing memory
   */
  def attach(attachParams: TSIHostWidgetAttachParams): (TLTSIHostWidget, TLIdentityNode) = {
    implicit val p = attachParams.p

    val name = "tsi_widget"
    val cbus = attachParams.controlBus

    // create a TL TSI Host Widget
    val tsiHostWidget = LazyModule(new TLTSIHostWidget(cbus.beatBytes, attachParams.tsiHostParams))
    tsiHostWidget.suggestName(name)

    // connect the manager (mmioNode) to the client (the bus wanted)
    cbus.coupleTo(s"slave_named_$name") {
      tsiHostWidget.mmioNode := TLFragmenter(cbus.beatBytes, cbus.blockBytes) := _
    }

    // connect the clock and reset
    InModuleBody { tsiHostWidget.module.clock := cbus.module.clock }
    InModuleBody { tsiHostWidget.module.reset := cbus.module.reset }

    // expose the widget and the client node
    (tsiHostWidget, tsiHostWidget.externalClientNode)
  }

  /**
   * Create a TSI Host Widget, attach it to the specified bus, then create an i/o to connect to
   *
   * @param attachParams params to connect with the widget to the bus
   * @param memoryBus the memory bus to connect the widget to
   * @return i/o to connect to the tsi widget
   */
  def attachAndMakePort(attachParams: TSIHostWidgetAttachParams, memoryBus: TLBusWrapper): ModuleValue[TSIHostWidgetIO] = {
    implicit val p = attachParams.p

    val (tsiHost, tsiHostMemNode) = attach(attachParams)

    // connect the widget mem node to the input membus
    memoryBus.coupleFrom(s"master_named_${tsiHost.name}") {
      _ := tsiHostMemNode
    }

    // create an io node that automatically makes an IO based on a bundle (can either make it a source or a sink)
    val tsiHostNode = BundleBridgeSource(() => new TSIHostWidgetIO(attachParams.tsiHostParams.serialIfWidth))
    InModuleBody { tsiHostNode.makeIO()(ValName(tsiHost.name)) }
  }

  /**
   * Tieoff all inputs to the widget
   *
   * @param port the i/o bundle interfacing with the widget
   * @param clock the clock to run the widget on
   * @return none
   */
  def tieoff(port: TSIHostWidgetIO, clock: Clock) {
    port.serial.in.bits := 0.U
    port.serial.in.valid := 0.U
    port.serial.out.ready := 0.U
    port.serial_clock := clock
  }

  /**
   * Connect the output to the input
   *
   * @param port the i/o bundle to connect
   * @return none
   */
  def loopback(port: TSIHostWidgetIO) {
    port.serial.in <> port.serial.out
  }
}
