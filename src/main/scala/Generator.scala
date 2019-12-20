package testchipip

import chisel3.{Module, Driver}
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.util.{GeneratorApp, HasGeneratorUtilities, ParsedInputNames}
import java.io.{File, FileWriter}
import firrtl.annotations.JsonProtocol

object Generator extends GeneratorApp {
  override lazy val longName = names.topModuleProject + "." + names.topModuleClass + "." + names.configs
  generateFirrtl
  generateAnno
}
