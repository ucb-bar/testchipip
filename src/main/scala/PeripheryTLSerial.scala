package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.{IO}
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._

case class AXIClockParams(
  clockFreqMHz: Double = 1000.0, // Match FireSim's 1GHz MBUS freq.
  crossingType: ClockCrossingType = AsynchronousCrossing() // Default to async crossing
)
case class AXIMemOverSerialTLClockParams(
  axiClockParams: Option[AXIClockParams] = Some(AXIClockParams()) // if set, axi port in different clk domain
) {
  def getMemFrequency(system: HasTileLinkLocations)(implicit p: Parameters): Double = {
    axiClockParams match {
      case Some(clkParams) => clkParams.clockFreqMHz * (1000 * 1000)
      case None => {
        // get the freq. from what the serial link masters
        system.locateTLBusWrapper(p(SerialTLAttachKey).masterWhere).dtsFrequency.get.toDouble
      }
    }
  }
}
case class SerialTLROMParams(
  address: BigInt = 0x20000,
  size: Int = 0x10000,
  contentFileName: Option[String] = None) // If unset, generates a JALR to DRAM_BASE

case class SerialTLParams(
  memParams: MasterPortParams,
  romParams: SerialTLROMParams = SerialTLROMParams(),
  isMemoryDevice: Boolean = false,
  width: Int = 4,
  receivesClock: Boolean = true,
  provideClockFreqMHz: Option[Int] = 60,
  credited: Option[Int] = Some(4), // if set, uses a credited interface with Int as the max number of credits
  axiMemOverSerialTLParams: Option[AXIMemOverSerialTLClockParams] = Some(AXIMemOverSerialTLClockParams()) // if enabled, expose axi port instead of TL RAM
)
case object SerialTLKey extends Field[Option[SerialTLParams]](None)

case class SerialTLAttachParams(
  masterWhere: TLBusWrapperLocation = FBUS,
  slaveWhere: TLBusWrapperLocation = MBUS,
  slaveCrossingType: ClockCrossingType = SynchronousCrossing()
)
case object SerialTLAttachKey extends Field[SerialTLAttachParams](SerialTLAttachParams())

trait CanHavePeripheryTLSerial { this: BaseSubsystem =>
  private val portName = "serial-tl"
  val (serdesser, serial_tl, serial_tl_clock_in, serial_tl_clock_out) = p(SerialTLKey).map { params =>
    val memParams = params.memParams
    val romParams = params.romParams
    val manager = locateTLBusWrapper(p(SerialTLAttachKey).slaveWhere) // The bus for which this acts as a manager
    val client = locateTLBusWrapper(p(SerialTLAttachKey).masterWhere) // The bus for which this acts as a client
    val memDevice = if (params.isMemoryDevice) new MemoryDevice else new SimpleDevice("lbwif-ram", Nil)
    val romDevice = new SimpleDevice("lbwif-rom", Nil)
    val clientPortParams = TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = "serial-tl",
        sourceId = IdRange(0, 1)
      ))
    )
    val managerPortParams = TLSlavePortParameters.v1(
      managers = Seq(
        TLSlaveParameters.v1(
          address            = AddressSet.misaligned(memParams.base, memParams.size),
          resources          = memDevice.reg,
          regionType         = RegionType.UNCACHED, // cacheable
          executable         = true,
          supportsGet        = TransferSizes(1, manager.blockBytes),
          supportsPutFull    = TransferSizes(1, manager.blockBytes),
          supportsPutPartial = TransferSizes(1, manager.blockBytes)
        ),
        TLSlaveParameters.v1(
          address            = List(AddressSet(romParams.address, romParams.size-1)),
          resources          = romDevice.reg,
          regionType         = RegionType.UNCACHED, // cacheable
          executable         = true,
          supportsGet        = TransferSizes(1, manager.blockBytes),
          fifoId             = Some(0)
        )
      ),
      beatBytes = memParams.beatBytes
    )


    val serdesser = client { LazyModule(new TLSerdesser(
      w = params.width,
      clientPortParams = clientPortParams,
      managerPortParams = managerPortParams
    )) }
    manager.coupleTo(s"port_named_serial_tl_mem") {
      ((client.crossIn(serdesser.managerNode)(ValName("TLSerialManagerCrossing")))(p(SerialTLAttachKey).slaveCrossingType)
        := TLSourceShrinker(1 << memParams.idBits)
        := TLWidthWidget(manager.beatBytes)
        := _ )
    }
    client.coupleFrom(s"port_named_serial_tl_ctrl") {
      ( _
        := TLBuffer()
        := serdesser.clientNode
      )
    }

    val hasInputClock = params.receivesClock
    val hasOutputClock = params.provideClockFreqMHz.isDefined
    val useCredited = params.credited.isDefined

    if (useCredited) {
      require(hasInputClock || hasOutputClock, "Credited serialTL must specify a clock direction, or both")
    } else {
      require(hasInputClock ^ hasOutputClock, "Decoupled serialTL must specify input ^ output clock"")
    }

    // If we provide a clock, generate a clock domain for the outgoing clock
    val serial_tl_clock_node = Option.when(hasOutputClock) { ClockSinkNode(Seq(ClockSinkParameters(
      take=Some(ClockParameters(params.provideClockFreqMHz.get))))) }
    serial_tl_clock_node.foreach(_ := ClockGroup()(p, ValName("serial_tl_clock")) := asyncClockGroupsNode)

    def creditedType = params.credited.map(c => new BidirCreditedIO(params.width, c)).get
    def decoupledType = new SerialIO(params.width)

    val inner_io = client { InModuleBody {
      val inner_io = IO(new ClockedAndResetIO(decoupledType)).suggestName("serial_tl")
      inner_io.bits <> serdesser.module.io.ser
      inner_io.clock := serdesser.module.clock
      inner_io.reset := serdesser.module.reset
      inner_io
    } }

    val serial_tl_clock_in = Option.when(hasInputClock) { InModuleBody {
      val clock_in = IO(Input(Clock()))
      clock_in
    } }

    val serial_tl_clock_out = Option.when(hasOutputClock) { InModuleBody {
      val clock_out = IO(Output(Clock()))
      clock_out := serial_tl_clock_node.get.in.head._1.clock
      clock_out
    } }

    // Handle async crossing here, the off-chip clock should only drive part of the Async Queue
    // The inner reset is the same as the serializer reset
    // The outer reset is the inner reset sync'd to the outer clock
    val outer_clock = InModuleBody { serial_tl_clock_in.getOrElse(serial_tl_clock_out.get).getWrappedValue }

    class PeripheryTLSerialCrossing extends RawModule {
      val io_inner = IO(Flipped(new ClockedAndResetIO(decoupledType)))
      val io_outer = IO(if (useCredited) creditedType else decoupledType)

      val io_outer_clock_inwards = Option.when(hasInputClock) { IO(Input(Clock())) }
      val io_outer_clock_outwards = Option.when(hasOutputClock) { IO(Input(Clock())) }

      val outer_reset = ResetCatchAndSync(
        io_outer_clock_inwards.getOrElse(io_outer_clock_outwards.get),
        io_inner.reset.asBool)

      val serial_out_async = Module(new AsyncQueue(UInt(params.width.W)))
      serial_out_async.io.enq <> io_inner.bits.out
      serial_out_async.io.enq_clock := io_inner.clock
      serial_out_async.io.enq_reset := io_inner.reset
      serial_out_async.io.deq_clock := io_outer_clock_outwards.getOrElse(io_outer_clock_inwards.get)
      serial_out_async.io.deq_reset := outer_reset

      val serial_in_async = Module(new AsyncQueue(UInt(params.width.W)))
      io_inner.bits.in <> serial_in_async.io.deq
      serial_in_async.io.enq_clock := io_outer_clock_inwards.getOrElse(io_outer_clock_outwards.get)
      serial_in_async.io.enq_reset := outer_reset
      serial_in_async.io.deq_clock := io_inner.clock
      serial_in_async.io.deq_reset := io_inner.reset

      if (useCredited) {
        val io_outer_credited = io_outer.asInstanceOf[BidirCreditedIO]
        val inwards_clock  = io_outer_clock_inwards.getOrElse(io_outer_clock_outwards.get)
        val outwards_clock = io_outer_clock_outwards.getOrElse(io_outer_clock_inwards.get)

        withClockAndReset(inwards_clock, outer_reset) {
          CreditedIO.fromReceiver(serial_in_async.io.enq, params.credited.get, false) <> BlockDuringReset(io_outer_credited.in)
        }
        withClockAndReset(outwards_clock, outer_reset) {
          io_outer_credited.out <> BlockDuringReset(CreditedIO.fromSender(serial_out_async.io.deq, params.credited.get, false))
        }
      } else {
        val io_outer_decoupled = io_outer.asInstanceOf[SerialIO]
        serial_in_async.io.enq <> io_outer_decoupled.in
        io_outer_decoupled.out <> serial_out_async.io.deq
      }
    }

    val serial_tl = InModuleBody {
      val serial_tl_crossing = Module(new PeripheryTLSerialCrossing)
      serial_tl_crossing.io_inner <> inner_io
      serial_tl_crossing.io_outer_clock_inwards.foreach(_ := serial_tl_clock_in.get)
      serial_tl_crossing.io_outer_clock_outwards.foreach(_ := serial_tl_clock_out.get)

      val data = IO(if (useCredited) creditedType else decoupledType)
      data <> serial_tl_crossing.io_outer
      data
    }

    (Some(serdesser), Some(serial_tl), serial_tl_clock_in, serial_tl_clock_out)
  }.getOrElse(None, None, None, None)
}
