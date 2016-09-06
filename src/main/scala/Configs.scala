package testchipip

import Chisel._
import junctions.unittests._
import rocketchip._
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}

class WithTestChipUnitTests extends Config(
  (pname, site, here) => pname match {
    case UnitTests => (testParams: Parameters) =>
      TestChipUnitTests(testParams)
  })

class TestChipUnitTestConfig extends Config(
  new WithTestChipUnitTests ++ new UnitTestConfig)

class WithTestChipSettings extends Config(
  (pname, site, here) => pname match {
    case TMemoryChannels => BusType.TL
  })

class DefaultTestChipConfig extends Config(
  new WithTestChipSettings ++ new BaseConfig)
