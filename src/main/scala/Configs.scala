package hurricaneip

import Chisel._
import junctions.unittests._
import rocketchip.UnitTestConfig
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}

class WithHurricaneUnitTests extends Config(
  (pname, site, here) => pname match {
    case UnitTests => (testParams: Parameters) =>
      HurricaneUnitTests(testParams)
  })

class HurricaneUnitTestConfig extends Config(
  new WithHurricaneUnitTests ++ new UnitTestConfig)
