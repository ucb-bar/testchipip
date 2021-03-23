package testchipip

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.subsystem.{BaseSubsystem, MasterPortParams}
import freechips.rocketchip.regmapper.{HasRegMap, RegFieldGroup}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.tile.{XLen}

import sifive.blocks.util.{NonBlockingEnqueue, NonBlockingDequeue}

import SerialAdapter._

/**
 * Parameters for the SerDes
 *
 * @param clientParams client parameters that receives the SerialIO inwards
 * @param managerParams manager parameters that sends the SerialIO outwards
 */
case class TSIHostSerdesParams(
  clientPortParams: TLMasterPortParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      name = "tl-tsi-host-serdes",
      sourceId = IdRange(0, 2)))),
  managerPortParams: TLSlavePortParameters = TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0, BigInt("FFFFFFFF", 16))), // access everything
      regionType = RegionType.UNCACHED,
      supportsGet = TransferSizes(1, 64),
      supportsPutFull = TransferSizes(1, 64),
      supportsPutPartial = TransferSizes(1, 64))),
    endSinkId = 0,
    beatBytes = 8),
  hasCorruptDenied: Boolean = true
)


/**
 * TSI Host parameter class
 *
 * @param offchipSerialIfWidth size of the serialIO out of the widget to external DUT or test chip (connects TLSerdessers)
 * @param txQueueEntries size of the queue for sending TSI requests
 * @param rxQueueEntries size of the queue for receiving TSI responses
 * @param mmioBaseAddress start address of the MMIO registers
 * @param mmioSourceId sourceId bits for MMIO TL node
 * @param serdesParams offchip serdes params
 * @param targetMasterPortParams host memory params
 */
case class TSIHostParams(
  offchipSerialIfWidth: Int = 32,
  txQueueEntries: Int = 16,
  rxQueueEntries: Int = 16,
  mmioBaseAddress: BigInt = BigInt("10017000", 16),
  mmioSourceId: Int = 2,
  serdesParams: TSIHostSerdesParams = TSIHostSerdesParams(),
  targetMasterPortParams: MasterPortParams = MasterPortParams(
    base = BigInt("80000000", 16),
    size = BigInt("10000000", 16),
    beatBytes = 8,
    idBits = 4)
)

/**
 * Offsets for the MMIO communication queues (base + offset to get the proper address)
 */
object TSIHostWidgetCtrlRegs {
  val txQueueOffset = 0x00
  val rxQueueOffset = 0x08
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

  val serial = new SerialIO(SerialAdapter.SERIAL_TSI_WIDTH)
}

/**
 * Mixin defining the module used to communicate between the MMIO and the TSI to TL converter
 */
trait TLTSIHostMMIOFrontendModule extends HasRegMap {
  implicit val p: Parameters

  val io: TLTSIHostMMIOFrontendBundle

  val params: TSIHostParams

  // tsi format input/output queues
  val txQueue = Module(new Queue(UInt(SerialAdapter.SERIAL_TSI_WIDTH.W), params.txQueueEntries)) // where is the queue being dequeued (to the SerialAdapter)
  val rxQueue = Module(new Queue(UInt(SerialAdapter.SERIAL_TSI_WIDTH.W), params.rxQueueEntries)) // where is the queue being enqueued (from the SerialAdapter)

  // connect queues to the backend
  io.serial.out <> txQueue.io.deq
  rxQueue.io.enq <> io.serial.in

  // require RV64
  require(p(XLen) == 64)

  // memory mapped registers and connections to the queues
  regmap(
    TSIHostWidgetCtrlRegs.txQueueOffset -> RegFieldGroup("txdata", Some("Transmit data"),
                                           NonBlockingEnqueue(txQueue.io.enq, 64)),
    TSIHostWidgetCtrlRegs.rxQueueOffset -> RegFieldGroup("rxdata", Some("Receive data"),
                                           NonBlockingDequeue(rxQueue.io.deq, 64))
  )
}

/**
 * Top level class that uses a register router to connect MMIO to the core. This instantiates
 * the implementation module and bundle
 *
 * @param beatBytesIn amount of bytes to send per beat
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
 * @param params the TSI parameters for the widget
 */
class TLTSIHostBackend(val params: TSIHostParams)(implicit p: Parameters)
  extends LazyModule
{
  // module to take in a decoupled io tsi stream and convert to a TL stream
  val serialAdapter = LazyModule(new SerialAdapter)
  // This converts the TL signals given by the serial adapter into a decoupled stream
  val serdes = LazyModule(new TLSerdesser(
        w = params.offchipSerialIfWidth,
        clientPortParams =  params.serdesParams.clientPortParams,
        managerPortParams = params.serdesParams.managerPortParams,
        hasCorruptDenied = params.serdesParams.hasCorruptDenied))

  // you are sending the TL request outwards... to the serdes manager... then to a serial stream... then to the real world (external DUT or test chip)
  serdes.managerNode := TLSourceSetter(params.mmioSourceId) := TLBuffer() := serialAdapter.node
  // send TL transaction to the memory system on this side
  // create TL node to connect to outer bus
  val externalClientNode = serdes.clientNode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val adapterSerial = new SerialIO(SerialAdapter.SERIAL_TSI_WIDTH)
      val serdesSerial =  new SerialIO(params.offchipSerialIfWidth)
    })

    val adapterMod = serialAdapter.module
    val serdesMod = serdes.module

    // connect MMIO to the encoder/decoder SerialAdapter
    io.adapterSerial.out <> adapterMod.io.serial.out
    adapterMod.io.serial.in <> io.adapterSerial.in

    // connect to the outside world
    serdesMod.io.ser.in <> io.serdesSerial.in // input decoupled to start serializing (from the target)
    io.serdesSerial.out <> serdesMod.io.ser.out // output of the SerialAdapter (sends a serialized stream to the target)
  }
}

/**
 * TSIHostWidget to connect the Front End SerVeR which sends TSI by an MMIO request to a target TL module
 * that resides across a SerialIO boundary often off-chip. Additionally, it allows for the target TL module
 * to access backing memory through the same SerialIO interface.
 *
 * MMIO TSI Request Flow:
 * TX Path: MMIO -> Queue -> SerialAdapter -> TL -> TLSerdesser Out
 * RX Path: TLSerdesser In -> SerialAdapter -> Queue -> MMIO
 *
 * Target TL Module Memory Request Flow:
 * TX Path: TLSerdesser In -> ExternalClient Node -> Host RAM
 * RX Path: Host RAM -> ExternalClient Node -> TLSerdesser Out
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
  val backend = LazyModule(new TLTSIHostBackend(params))

  // create TL node for MMIO
  val mmioNode = TLIdentityNode()
  // create TL node to connect to outer bus
  val externalClientNode = TLIdentityNode()

  // set up async fifos
  val mmioSink = LazyModule(new TLAsyncCrossingSink)
  val clientSource = LazyModule(new TLAsyncCrossingSource)

  // connect and simplify the MMIO frontend TL
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
  val ioNode = BundleBridgeSource(() => new TSIHostWidgetIO(params.offchipSerialIfWidth))

  lazy val module = new LazyModuleImp(this) {
    val io = ioNode.bundle

    val backendMod = backend.module
    val mmioMod = mmioFrontend.module
    val mmioSinkMod = mmioSink.module
    val clientSourceMod = clientSource.module

    val syncReset = ResetCatchAndSync(io.serial_clock, reset.asBool)

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
    val tsiHostNode = BundleBridgeSource(() => new TSIHostWidgetIO(attachParams.tsiHostParams.offchipSerialIfWidth))
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
    port.serial.in.valid := false.B
    port.serial.out.ready := true.B
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
