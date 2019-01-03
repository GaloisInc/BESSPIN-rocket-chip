
package galois.subsystem

import Chisel._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.HasResetVectorWire

/** Subsystem will power-on running at 0x10040 (BootROM) */
trait HasGaloisGFEResetVectorImp extends LazyModuleImp
  with HasResetVectorWire {
  global_reset_vector := 0x80000000L.U
}