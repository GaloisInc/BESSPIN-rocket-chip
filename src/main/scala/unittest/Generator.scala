// See LICENSE.SiFive for license details.

package ssithchips.rocketchip.unittest

object Generator extends ssithchips.rocketchip.util.GeneratorApp {
  val longName = names.topModuleProject + "." + names.configs
  generateFirrtl
  generateAnno
  generateTestSuiteMakefrags // TODO: Needed only for legacy make targets
  generateArtefacts
}
