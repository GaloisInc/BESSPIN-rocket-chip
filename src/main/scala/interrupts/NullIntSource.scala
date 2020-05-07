// See LICENSE.SiFive for license details.

package ssithchips.rocketchip.interrupts

import Chisel._
import ssithchips.rocketchip.config.Parameters
import ssithchips.rocketchip.diplomacy._

/** Useful for stubbing out parts of an interrupt interface where certain devices might be missing */
class NullIntSource(num: Int = 1, ports: Int = 1, sources: Int = 1)(implicit p: Parameters) extends LazyModule
{
  val intnode = IntSourceNode(IntSourcePortSimple(num, ports, sources))

  lazy val module = new LazyModuleImp(this) {
    intnode.out.foreach { case (o, _) => o.foreach { _ := false.B } }
  }
}

object NullIntSource {
  def apply(num: Int = 1, ports: Int = 1, sources: Int = 1)(implicit p: Parameters): IntNode = {
    val null_int_source = LazyModule(new NullIntSource(num, ports, sources))
    null_int_source.intnode
  }
}
