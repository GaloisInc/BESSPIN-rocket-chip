// See LICENSE.SiFive for license details.

package galois.devices

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.devices.tilelink.PLICParams


case object ExtPLICKey extends Field[Option[PLICParams]](None)

/** Trait that will connect an external PLIC to a subsystem */
trait CanHaveExtPLIC { this: BaseSubsystem =>
  val extPlicOpt  = None
  //p(ExtPLICKey).map { params =>
  //  val plic = None
  //  plic
  //}
}
