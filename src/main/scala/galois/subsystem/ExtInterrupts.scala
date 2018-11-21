// See LICENSE.SiFive for license details.

package galois.subsystem

import Chisel._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.BaseSubsystem
import galois.devices._

/** This trait adds externally driven interrupts to the system.
  * However, it should not be used directly; instead one of the below
  * synchronization wiring child traits should be used.
  */
trait HasExtPLIC { this: BaseSubsystem =>
  val extPLIC = true
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasExtPLICBundle {
  val interrupts: UInt

  def tieOffInterrupts(dummy: Int = 1) {
    interrupts := UInt(0)
  }
}

/** This trait performs the translation from a UInt IO into Diplomatic Interrupts.
  * The wiring must be done in the concrete LazyModuleImp.
  */
trait HasExtPLICModuleImp extends LazyModuleImp with HasExtPLICBundle {
  val outer: HasExtPLIC
  val interrupts = IO(UInt(INPUT, width = 3))

  //  outer.extInterrupts.out.map(_._1).flatten.zipWithIndex.foreach { case(o, i) => o := interrupts(i) }
}

trait HasExtCLINTBundle {
  val clint_interrupts: UInt

  def tieOffClintInterrupts(dummy: Int = 1): Unit = {
    clint_interrupts := UInt(0)
  }
}

trait HasExtCLINTModuleImp extends LazyModuleImp with HasExtCLINTBundle {
  val outer: BaseSubsystem with CanHaveExtCLINT
  val clint_interrupts = IO(UInt(INPUT, width = 2))

  outer.extClintOpt.foreach { clint =>
    clint.module.io.clint_extint := clint_interrupts
  }
}