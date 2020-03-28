package testchipip

import chisel3._
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.subsystem.{BuildSystemBus, SystemBusKey}
import freechips.rocketchip.unittest.UnitTests

class WithRingSystemBus(
    buffer: TLNetworkBufferParams = TLNetworkBufferParams.default)
    extends Config((site, here, up) => {
  case BuildSystemBus => (p: Parameters) =>
    new RingSystemBus(p(SystemBusKey), buffer)(p)
})

class WithTestChipUnitTests extends Config((site, here, up) => {
  case UnitTests => (testParams: Parameters) =>
    TestChipUnitTests(testParams)
})

class TestChipUnitTestConfig extends Config(
  new WithTestChipUnitTests ++ new BaseConfig)

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
