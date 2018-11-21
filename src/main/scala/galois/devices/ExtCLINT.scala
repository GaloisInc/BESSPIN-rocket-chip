package galois.devices

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.util.Annotated

case object ExtCLINTKey extends Field[Option[CLINTParams]](None)

class ExtCLINT(params: CLINTParams)(implicit p: Parameters) extends LazyModule
{
  import CLINTConsts._
  //  override val node = 0.U

  val device = new SimpleDevice("clint", Seq("riscv,clint0")) {
    override val alwaysExtended = true
  }

  val intnode = IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(ints, Seq(Resource(device, "int"))))) },
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false)

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

/** Trait that will connect a CLINT to a subsystem */
trait CanHaveExtCLINT { this: BaseSubsystem =>
  val extClintOpt = p(ExtCLINTKey).map { params =>
    val clint = LazyModule(new ExtCLINT(params))
    clint
  }
}