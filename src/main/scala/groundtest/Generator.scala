// See LICENSE.SiFive for license details.

package ssithchips.rocketchip.groundtest

import ssithchips.rocketchip.util.GeneratorApp

object Generator extends GeneratorApp {
  val longName = names.topModuleProject + "." + names.configs
  generateFirrtl
  generateAnno
  generateTestSuiteMakefrags // TODO: Needed only for legacy make targets
  generateArtefacts
}
