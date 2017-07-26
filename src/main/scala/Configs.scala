package testchipip

import chisel3._
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.unittest.UnitTests

class WithTestChipUnitTests extends Config((site, here, up) => {
  case UnitTests => (testParams: Parameters) =>
    TestChipUnitTests(testParams)
})

class TestChipUnitTestConfig extends Config(
  new WithTestChipUnitTests ++ new BaseConfig)

class WithBlockDevice extends Config((site, here, up) => {
  case BlockDeviceKey => BlockDeviceConfig()
})

class WithNBlockDeviceTrackers(n: Int) extends Config((site, here, up) => {
  case BlockDeviceKey => up(BlockDeviceKey, site).copy(nTrackers = n)
})
