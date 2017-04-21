package testchipip

object Generator extends util.GeneratorApp {
  val longName = names.topModuleClass + "." + names.configs
  generateFirrtl
}
