package testchipip

import freechips.rocketchip.config.Parameters
import firrtl.options.{StageMain}
import freechips.rocketchip.system.{RocketChipStage}

object Generator extends StageMain(new RocketChipStage)
