package testchipip

import freechips.rocketchip.util.GeneratorApp

object Generator extends GeneratorApp {
  val longName = names.topModuleProject + "." + names.configs
  generateFirrtl
  generateAnno
}
