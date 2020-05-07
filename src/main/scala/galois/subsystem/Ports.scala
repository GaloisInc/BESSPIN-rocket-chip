
package galois.subsystem

import Chisel._
import ssithchips.rocketchip.diplomacy._
import ssithchips.rocketchip.subsystem._

/** Subsystem will power-on running at 0xC000_0000 (AXI Boot ROM) */
trait HasGaloisGFEResetVectorImp extends LazyModuleImp
  with HasResetVectorWire {
  global_reset_vector := 0x70000000L.U
}

trait CanHaveGFEMasterAXI4MemPortModuleImp extends CanHaveMasterAXI4MemPortModuleImp {
  val outer: CanHaveMasterAXI4MemPort

  override def connectSimAXIMem() = {
    (mem_axi4 zip outer.memAXI4Node).foreach { case (io, node) =>
      (io zip node.in).foreach { case (io, (_, edge)) =>
        val mem = LazyModule(new SimAXIMem(edge, size = p(ExtMem).get.master.size / 8))
        Module(mem.module).io.axi4.head <> io
      }
    }
  }
}
