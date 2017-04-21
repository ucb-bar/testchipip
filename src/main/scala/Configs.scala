package testchipip

import chisel3._
import unittest.UnitTests
import rocketchip.BaseConfig
import uncore.tilelink.TLId
import config.{Parameters, Config}

class WithTestChipUnitTests extends Config((site, here, up) => {
  case UnitTests => (testParams: Parameters) =>
    TestChipUnitTests(testParams)
  case TLId => "L1toL2"
})

class TestChipUnitTestConfig extends Config(
  new WithTestChipUnitTests ++ new BaseConfig)

class WithSerialAdapter extends Config((site, here, up) => {
  case SerialInterfaceWidth => 32
})
