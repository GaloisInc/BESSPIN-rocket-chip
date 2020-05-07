// See LICENSE.SiFive for license details.

package ssithchips.rocketchip.interrupts

import Chisel._
import ssithchips.rocketchip.config.Parameters
import ssithchips.rocketchip.diplomacy._

class IntXbar()(implicit p: Parameters) extends LazyModule
{
  val intnode = IntNexusNode(
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    sourceFn       = { seq =>
      IntSourcePortParameters((seq zip seq.map(_.num).scanLeft(0)(_+_).init).map {
        case (s, o) => s.sources.map(z => z.copy(range = z.range.offset(o)))
      }.flatten)
    })

  lazy val module = new LazyModuleImp(this) {
    val cat = intnode.in.map { case (i, e) => i.take(e.source.num) }.flatten
    intnode.out.foreach { case (o, _) => o := cat }
  }
}
