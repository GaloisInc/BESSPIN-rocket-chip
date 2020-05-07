// See LICENSE.SiFive for license details.

package ssithchips.rocketchip.diplomacy

import scala.language.experimental.macros
import ssithchips.rocketchip.macros.ValNameImpl

case class ValName(name: String)

object ValName
{
  implicit def materialize(implicit x: ValNameImpl): ValName = ValName(x.name)
}
