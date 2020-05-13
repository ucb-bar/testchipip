package testchipip

import chisel3._
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.tilelink.{TLBusWrapperTopology}
import freechips.rocketchip.subsystem.{TLNetworkTopologyLocated, InSubsystem, SBUS, JustOneBusTopologyParams, SystemBusParams}
import freechips.rocketchip.unittest.UnitTests

class WithRingSystemBus(
    buffer: TLNetworkBufferParams = TLNetworkBufferParams.default)
    extends Config((site, here, up) => {
  case TLNetworkTopologyLocated(InSubsystem) =>
    up(TLNetworkTopologyLocated(InSubsystem), site).map(topo =>
      topo match {
        case j: JustOneBusTopologyParams =>
          new TLBusWrapperTopology(j.instantiations.map(inst => inst match {
            case (SBUS, sbus_params: SystemBusParams) => (SBUS, RingSystemBusParams(sbus_params, buffer))
            case a => a
          }
        ), j.connections)
        case x => x
      }
    )
})

class WithTestChipUnitTests extends Config((site, here, up) => {
  case UnitTests => (testParams: Parameters) =>
    TestChipUnitTests(testParams)
})

class WithClockUtilTests extends Config((site, here, up) => {
  case UnitTests => (testParams: Parameters) => ClockUtilTests()
})

class TestChipUnitTestConfig extends Config(
  new WithTestChipUnitTests ++ new BaseConfig)

class ClockUtilTestConfig extends Config(
  new WithClockUtilTests ++ new BaseConfig)

class WithBlockDevice extends Config((site, here, up) => {
  case BlockDeviceKey => Some(BlockDeviceConfig())
})

class WithNBlockDeviceTrackers(n: Int) extends Config((site, here, up) => {
  case BlockDeviceKey => up(BlockDeviceKey, site) match {
    case Some(a) => Some(a.copy(nTrackers = n))
    case None => None
  }
})

class WithTSI extends Config((site, here, up) => {
  case SerialKey => true
})
