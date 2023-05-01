package testchipip

import chisel3._
import chisel3.util._
import chisel3.experimental.{IO, DataMirror}
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.debug.HasPeripheryDebug
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.prci._
import scala.math.min
import freechips.rocketchip.amba.axi4._

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
        system.locateTLBusWrapper(p(SerialTLKey).get.attachParams.masterWhere).dtsFrequency.get.toDouble
      }
    }
  }
}
case class SerialTLROMParams(
  address: BigInt = 0x20000,
  size: Int = 0x10000,
  contentFileName: Option[String] = None) // If unset, generates a JALR to DRAM_BASE

case class SerialTLManagerParams(
  memParams: MasterPortParams,
  romParams: Option[SerialTLROMParams] = None,
  isMemoryDevice: Boolean = false,
  axiMemOverSerialTLParams: Option[AXIMemOverSerialTLClockParams] = Some(AXIMemOverSerialTLClockParams()) // if enabled, expose axi port instead of TL RAM
)

// The SerialTL can be configured to be bidirectional if serialManagerParams is set
case class SerialTLParams(
  serialManagerParams: Option[SerialTLManagerParams],
  width: Int = 4,
  attachParams: SerialTLAttachParams = SerialTLAttachParams(),
  provideClock: Boolean = false
)
case object SerialTLKey extends Field[Option[SerialTLParams]](None)

case class SerialTLAttachParams(
  masterWhere: TLBusWrapperLocation = FBUS,
  slaveWhere: TLBusWrapperLocation = OBUS,
  slaveCrossingType: ClockCrossingType = SynchronousCrossing()
)

trait CanHavePeripheryTLSerial { this: BaseSubsystem =>
  private val portName = "serial-tl"
  val (serdesser, serial_tl) = p(SerialTLKey).map { params =>
    val attachParams = params.attachParams
    lazy val manager = locateTLBusWrapper(attachParams.slaveWhere) // The bus for which this acts as a manager
    lazy val client = locateTLBusWrapper(attachParams.masterWhere) // The bus for which this acts as a client
    val clientPortParams = TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = "serial-tl",
        sourceId = IdRange(0, 1)
      ))
    )
    require(clientPortParams.clients.size == 1)

    // Assume we are in the same domain as our client-side binding.
    val serial_tl_domain = LazyModule(new ClockSinkDomain(name=Some(portName)))
    serial_tl_domain.clockNode := client.fixedClockNode

    val managerPortParams = params.serialManagerParams.map { managerParams =>
      val memParams = managerParams.memParams
      val romParams = managerParams.romParams
      val memDevice = if (managerParams.isMemoryDevice) new MemoryDevice else new SimpleDevice("lbwif-ram", Nil)
      val romDevice = new SimpleDevice("lbwif-rom", Nil)
      TLSlavePortParameters.v1(
        managers = Seq(
          TLSlaveParameters.v1(
            address            = AddressSet.misaligned(memParams.base, memParams.size),
            resources          = memDevice.reg,
            regionType         = RegionType.UNCACHED, // cacheable
            executable         = true,
            supportsGet        = TransferSizes(1, manager.blockBytes),
            supportsPutFull    = TransferSizes(1, manager.blockBytes),
            supportsPutPartial = TransferSizes(1, manager.blockBytes)
          )
        ) ++ romParams.map { romParams =>
          TLSlaveParameters.v1(
            address            = List(AddressSet(romParams.address, romParams.size-1)),
            resources          = romDevice.reg,
            regionType         = RegionType.UNCACHED, // cacheable
            executable         = true,
            supportsGet        = TransferSizes(1, manager.blockBytes),
            fifoId             = Some(0)
          )
        },
        beatBytes = memParams.beatBytes
      )
    }

    val serdesser = serial_tl_domain { LazyModule(new TLSerdesser(
      w = params.width,
      clientPortParams = Some(clientPortParams),
      managerPortParams = managerPortParams
    )) }
    serdesser.managerNode.foreach { managerNode =>
      manager.coupleTo(s"port_named_serial_tl_mem") {
        ((serial_tl_domain.crossIn(managerNode)(ValName("TLSerialManagerCrossing")))(attachParams.slaveCrossingType)
          := TLSourceShrinker(1 << params.serialManagerParams.get.memParams.idBits)
          := TLWidthWidget(manager.beatBytes)
          := _ )
      }
    }
    client.coupleFrom(s"port_named_serial_tl_ctrl") { _ := TLBuffer() := serdesser.clientNode.get }

    def serialType = if (params.provideClock) {
      new ClockedIO(new SerialIO(params.width))
    } else {
      Flipped(new ClockedIO(Flipped(new SerialIO(params.width))))
    }

    val inner_io = serial_tl_domain { InModuleBody {
      val inner_io = IO(serialType).suggestName("serial_tl")
      if (params.provideClock) {
        // no async crossing necessary, since the output pins are clocked with the serdesser
        inner_io.clock := serdesser.module.clock
        inner_io.bits.out <> BlockDuringReset(serdesser.module.io.ser.out, 4)
        serdesser.module.io.ser.in <> BlockDuringReset(inner_io.bits.in, 4)
      } else {
        // Handle async crossing here, the off-chip clock should only drive part of the Async Queue
        // The inner reset is the same as the serializer reset
        // The outer reset is the inner reset sync'd to the outer clock
        val outer_reset = ResetCatchAndSync(inner_io.clock, serdesser.module.reset.asBool)
        val out_async = Module(new AsyncQueue(UInt(params.width.W)))
        out_async.io.enq <> BlockDuringReset(serdesser.module.io.ser.out, 4)
        out_async.io.enq_clock := serdesser.module.clock
        out_async.io.enq_reset := serdesser.module.reset
        out_async.io.deq_clock := inner_io.clock
        out_async.io.deq_reset := outer_reset

        val in_async = Module(new AsyncQueue(UInt(params.width.W)))
        in_async.io.enq <> BlockDuringReset(inner_io.bits.in, 4)
        in_async.io.enq_clock := inner_io.clock
        in_async.io.enq_reset := outer_reset
        in_async.io.deq_clock := serdesser.module.clock
        in_async.io.deq_reset := serdesser.module.reset

        inner_io.bits.out          <> out_async.io.deq
        serdesser.module.io.ser.in <> in_async.io.deq
      }

      inner_io
    } }
    val outer_io = InModuleBody {
      val outer_io = IO(serialType).suggestName("serial_tl")
      outer_io <> inner_io
      outer_io
    }
    (Some(serdesser), Some(outer_io))
  }.getOrElse(None, None)
}
