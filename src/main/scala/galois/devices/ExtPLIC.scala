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

class ExtPLIC(params: PLICParams)(implicit p: Parameters) extends LazyModule
{

  val device = new SimpleDevice("plic", Seq("riscv,plic0")) {
    override val alwaysExtended = true
  }

  val intnode = IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1, Seq(Resource(device, "int"))))) },
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false,
    inputRequiresOutput = false)

  lazy val module = new LazyModuleImp(this) {
    Annotated.params(this, params)
    require (intnode.edges.in.size == 0, "CLINT only produces interrupts; it does not accept them")

    val io = IO(new Bundle {
      val clint_extint = UInt(INPUT, width = 2)
    })

    val (intnode_out, _) = intnode.out.unzip
    intnode_out.zipWithIndex.foreach { case (int, i) =>
      int(0) := io.clint_extint(0) // msip
      int(1) := io.clint_extint(1) // mtip
    }
  }
}

/** Trait that will connect an external PLIC to a subsystem */
trait CanHaveExtPLIC { this: BaseSubsystem =>
  val extPlicOpt  = None
  //p(ExtPLICKey).map { params =>
  //  val plic = None
  //  plic
  //}
}
