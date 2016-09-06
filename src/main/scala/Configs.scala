package testchipip

import Chisel._
import junctions.unittests._
import rocketchip.UnitTestConfig
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}

class WithTestChipUnitTests extends Config(
  (pname, site, here) => pname match {
    case UnitTests => (testParams: Parameters) =>
      TestChipUnitTests(testParams)
  })

class TestChipUnitTestConfig extends Config(
  new WithTestChipUnitTests ++ new UnitTestConfig)
