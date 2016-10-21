package testchipip

import Chisel._
import unittest.UnitTests
import rocketchip.{BaseConfig, NCoreplexExtClients}
import uncore.tilelink.TLId
import cde.{Parameters, Config, CDEMatchError}

class WithTestChipUnitTests extends Config(
  (pname, site, here) => pname match {
    case NCoreplexExtClients => 0
    case UnitTests => (testParams: Parameters) =>
      TestChipUnitTests(testParams)
    case TLId => "L1toL2"
    case _ => throw new CDEMatchError
  })

class TestChipUnitTestConfig extends Config(
  new WithTestChipUnitTests ++ new BaseConfig)

class WithSerialAdapter extends Config(
  (pname, site, here) => pname match {
    case SerialInterfaceWidth => 32
    case _ => throw new CDEMatchError
  })
