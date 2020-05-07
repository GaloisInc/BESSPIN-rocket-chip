// See LICENSE.SiFive for license details.
// Galois, Inc

package galois.devices

import Chisel._
import ssithchips.rocketchip.config.Parameters
import ssithchips.rocketchip.diplomacy._
import ssithchips.rocketchip.amba.axi4._

class AXI4Tap()(implicit p: Parameters) extends LazyModule
{
  val node = AXI4AdapterNode(
    masterFn = { p => p },
    slaveFn  = { p => p })

  lazy val module = new LazyModuleImp(this) {
    // TLtoAXI master is "in"
    // Memory is "out"
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.aw <> in .aw
      out.w  <> in .w
      in .b  <> out.b
      out.ar <> in .ar
      in .r  <> out.r

//        in.ar.ready := Wire(Bool(false))
//        in.r.ready := Wire(Bool(false))
    }
  }
}

object AXI4Tap
{
  def apply()(implicit p: Parameters): AXI4Node =
  {
    val axi4tap = LazyModule(new AXI4Tap())
    axi4tap.node
  }
}
