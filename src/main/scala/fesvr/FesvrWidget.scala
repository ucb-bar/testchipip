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
 * FESVR parameter class
 *
 * @param mmioRegWidth size of the MMIO data being sent back and forth in bits
 * @param serialIfWidth size of the serialIO *out* of the widget
 * @param txQueueEntries size of the queue for sending TSI requests
 * @param rxQueueEntries size of the queue for recieving TSI responses
 * @param baseAddress start address of the MMIO registers
 * @param tlAddressSet set of addresses that you can access on the target side
 * @param tlRegionType is cache of TL transcations happening on this side?
 * @param tlNumTransactions is the number of inflight transactions that can happen (also determines the id associated with that transaction)
 * @param tlLineBytes you can transfer up to a line at a time (this determines the amount of beats in a TL burst)
 * @param bypassMMIO disconnect the mmio and have a bypass port that connects directly to the SerialAdapter
 */
case class FESVRParams(
  mmioRegWidth: Int = 32,
  serialIfWidth: Int = 32,
  txQueueEntries: Int = 16,
  rxQueueEntries: Int = 16,
  baseAddress: BigInt = BigInt(0x10017000),
  tlAddressSet: AddressSet = AddressSet(0, BigInt("FFFFFFFF", 16)),
  tlRegionType: RegionType.T = RegionType.UNCACHED,
  tlNumTransactions: Int = 1,
  tlLineBytes: Int = 64,
  bypassMMIO: Boolean = false
)

/**
 * Offsets for the mmio communication queues (base + offset to get the proper address)
 */
object FESVRWidgetCtrlRegs {
  val txQueueOffset = 0x00
  val rxQueueOffset = 0x04
}

/**
 * I/O to the outside world. This is the stream data out of the FESVRWidget.
 *
 * @param w width in bits of connection to outside world
 */
class FESVRWidgetIO(val w: Int)(implicit val p: Parameters) extends Bundle {
  val serial = new SerialIO(w)
  val debug: SerialIO = if(p(PeripheryFESVRKey).bypassMMIO) { new SerialIO(w) } else { null } // unconnected in normal operation
}

/**
 * I/O bundle to connect to the mmio interaction class
 */
trait TLFESVRMMIOBundle {
  implicit val p: Parameters
  val serial = new SerialIO(p(PeripheryFESVRKey).mmioRegWidth)
}

/**
 * Mixin defining the module used to communicate between the MMIO and the parser
 */
trait TLFESVRMMIOModule extends HasRegMap {
  implicit val p: Parameters

  val io: TLFESVRMMIOBundle

  val txQueue = Module(new Queue(UInt(p(PeripheryFESVRKey).mmioRegWidth.W), p(PeripheryFESVRKey).txQueueEntries)) // where is the queue being dequeued (to the parser serializer)
  val rxQueue = Module(new Queue(UInt(p(PeripheryFESVRKey).mmioRegWidth.W), p(PeripheryFESVRKey).rxQueueEntries)) // where is the queue being enqueued (from the parser deserializer)

  io.serial.out <> txQueue.io.deq
  rxQueue.io.enq <> io.serial.in

  // memory mapped registers and connections to the queues
  regmap(
    FESVRWidgetCtrlRegs.txQueueOffset -> Seq(RegField.w(p(PeripheryFESVRKey).mmioRegWidth, txQueue.io.enq)),
    FESVRWidgetCtrlRegs.rxQueueOffset -> Seq(RegField.r(p(PeripheryFESVRKey).mmioRegWidth, rxQueue.io.deq))
  )
}

/**
 * Top level class that uses a register router to connect MMIO to the core. This instantes
 * the implementation module and bundle
 *
 * @param beatBytes amount of bytes to send per beat
 */
class TLFESVRMMIO(val beatBytesIn: Int)(implicit p: Parameters)
  extends TLRegisterRouter(
    base = p(PeripheryFESVRKey).baseAddress,
    devname = "fesvr-mmio",
    devcompat = Seq("ucbbar,fesvr-widget"),
    beatBytes = beatBytesIn)(
      new TLRegBundle(p(PeripheryFESVRKey), _)    with TLFESVRMMIOBundle)(
      new TLRegModule(p(PeripheryFESVRKey), _, _) with TLFESVRMMIOModule)

/**
 * FESVRWidget to connect the Front End SerVeR to a target TL module.
 * Resides on the Rocket-Chip periphery.
 *
 * Tx is MMIO -> Queue -> Serializer -> TL
 * Rx is TL -> Deserializer -> Queue -> MMIO
 *
 * @param fesvrParams parameter object
 */
class TLFESVRWidget(val beatBytes: Int)(implicit p: Parameters)
  extends LazyModule
{
  // this should communicate over MMIO to the core (tx and rx) and
  // should covert the communication from TL (aka TL -> Parse -> MMIO, MMIO -> Parse -> TL)
  val mmio: TLFESVRMMIO = if (p(PeripheryFESVRKey).bypassMMIO) { null } else { LazyModule(new TLFESVRMMIO(beatBytes)) }
  // reuse module from Howie to take in a decoupled io tsi and convert to a TL stream
  val parseToTL = LazyModule(new SerialAdapter)
  // This converts the TL signals given by the parser into a decoupled stream
  val serdes = LazyModule(new TLSerdesser(
        w = p(PeripheryFESVRKey).serialIfWidth,
        clientParams = TLClientParameters(
          name = "tl-ser",
          sourceId = IdRange(0, p(PeripheryFESVRKey).tlNumTransactions)),
        managerParams = TLManagerParameters(
          address = Seq(p(PeripheryFESVRKey).tlAddressSet),
          regionType = p(PeripheryFESVRKey).tlRegionType,
          supportsGet = TransferSizes(1, p(PeripheryFESVRKey).tlLineBytes),
          supportsPutFull = TransferSizes(1, p(PeripheryFESVRKey).tlLineBytes),
          supportsPutPartial = TransferSizes(1, p(PeripheryFESVRKey).tlLineBytes)),
        beatBytes = beatBytes))

  // currently the amount of data out of the mmio regs should equal the serial IO
  require(p(PeripheryFESVRKey).serialIfWidth == SERIAL_IF_WIDTH)

  // create TL node for MMIO
  val mmioNode: TLIdentityNode = if (p(PeripheryFESVRKey).bypassMMIO) { null } else { TLIdentityNode() }
  // create TL node to connect to outer bus
  val externalClientNode = TLIdentityNode()

  // setup the TL connection graph
  if (!(p(PeripheryFESVRKey).bypassMMIO)) {
    mmio.node := TLAtomicAutomata() := mmioNode
  }
  // you are sending the TL request outwards... to the serdes manager... then to a serial stream
  serdes.managerNode := TLBuffer() := parseToTL.node
  // send TL transaction to the memory system on this side
  externalClientNode := TLBuffer() := serdes.clientNode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new FESVRWidgetIO(p(PeripheryFESVRKey).serialIfWidth))

    val adapterMod = parseToTL.module
    val serdesMod = serdes.module

    // connect mmio to the parser
    if (!(p(PeripheryFESVRKey).bypassMMIO)) {
      val mmioMod = mmio.module
      mmioMod.io.serial.in <> Queue(adapterMod.io.serial.out)
      adapterMod.io.serial.in <> Queue(mmioMod.io.serial.out)
    }
    else {
      io.debug.out <> Queue(adapterMod.io.serial.out)
      adapterMod.io.serial.in <> Queue(io.debug.in)
    }

    // connect to the outside world
    serdesMod.io.ser.in <> io.serial.in // input decoupled to start serializing (this comes from the outside world)
    io.serial.out <> serdesMod.io.ser.out // output of the serializer (sends a serialized stream to the outside world)
  }
}
