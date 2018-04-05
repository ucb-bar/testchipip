package testchipip

import chisel3.{Module, Driver}
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.util.{HasGeneratorUtilities, ParsedInputNames}
import java.io.{File, FileWriter}
import net.jcazevedo.moultingyaml._
import firrtl.annotations.AnnotationYamlProtocol._

trait GeneratorApp extends App with HasGeneratorUtilities {
  lazy val names = ParsedInputNames(
    targetDir = args(0),
    topModuleProject = args(1),
    topModuleClass = args(2),
    configProject = args(3),
    configs = args(4))

  lazy val config: Config = getConfig(names.fullConfigClasses)
  lazy val params: Parameters = config.toInstance
  lazy val circuit = Driver.elaborate(() =>
      Class.forName(names.fullTopModuleClass)
        .getConstructor(classOf[Parameters])
        .newInstance(params)
        .asInstanceOf[Module])

  lazy val longName = names.topModuleProject + "." +
                 names.topModuleClass + "." +
                 names.configs

  def generateFirrtl =
    Driver.dumpFirrtl(circuit,
      Some(new File(names.targetDir, s"$longName.fir")))

  def generateAnno {
    val annoFile = new File(names.targetDir, s"$longName.anno")
    val afw = new FileWriter(annoFile)
    afw.write(circuit.annotations.toArray.toYaml.prettyPrint)
    afw.close()
  }
}

object Generator extends GeneratorApp {
  generateFirrtl
  generateAnno
}
