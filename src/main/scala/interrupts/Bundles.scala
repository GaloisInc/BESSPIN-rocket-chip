// See LICENSE.SiFive for license details.

package ssithchips.rocketchip.interrupts

import Chisel._
import ssithchips.rocketchip.diplomacy._
import ssithchips.rocketchip.util._

class SyncInterrupts(params: IntEdge) extends GenericParameterizedBundle(params)
{
  val sync = Vec(params.source.num, Bool())
}
