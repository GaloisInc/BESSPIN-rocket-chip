// See LICENSE.SiFive for license details.

package galois.subsystem

import Chisel._
import freechips.rocketchip.devices.debug.TLDebugModule
import freechips.rocketchip.devices.tilelink.{CLINT, CLINTConsts, CLINTKey, PLICKey, TLPLIC}
import freechips.rocketchip.interrupts._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tile.BaseTile
import galois.devices.{ExtCLINT, ExtCLINTKey, ExtPLICKey}

trait HasTiles extends freechips.rocketchip.subsystem.HasTiles { this: BaseSubsystem =>

  val clintNode = p(CLINTKey) match {
    case Some(_) => None
    case None    => Some(IntNexusNode(
      sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(2, Seq(Resource(new SimpleDevice("clint", Seq("riscv,clint0")) {
        override val alwaysExtended = true
      }, "int"))))) },
      sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
      outputRequiresInput = false))
  }
  // Ignoring interrupts from Tile for now. We don't have any yet
//  val testNode = IntNexusNode(
//    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1))) },
//    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
//    outputRequiresInput = false,
//    inputRequiresOutput = false)
//
//  val plicOutNode = p(PLICKey) match {
//    case Some(_) => None
//    case None    => Some(IntNexusNode(
//      sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1))) },
//      sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
//      outputRequiresInput = false,
//      inputRequiresOutput = false))
//  }

   override protected def connectInterrupts(tile: BaseTile, debugOpt: Option[TLDebugModule], clintOpt: Option[CLINT], plicOpt: Option[TLPLIC]) {
    // Handle all the different types of interrupts crossing to or from the tile:
    // NOTE: The order of calls to := matters! They must match how interrupts
    //       are decoded from tile.intInwardNode inside the tile. For this reason,
    //       we stub out missing interrupts with constant sources here.

     // 1. Debug interrupt is definitely asynchronous in all cases.
    tile.intInwardNode :=
      debugOpt
        .map { tile { IntSyncCrossingSink(3) } := _.intnode }
        .getOrElse { NullIntSource() }

    // 2. The CLINT and PLIC output interrupts are synchronous to the TileLink bus clock,
    //    so might need to be synchronized depending on the Tile's crossing type.

    //    From CLINT: "msip" and "mtip"
    // Prioritize the internal CLINT if we somehow instantiated both
    tile.crossIntIn() :=
      clintOpt.map { _.intnode }.getOrElse {
        clintNode.getOrElse {
          NullIntSource(sources = CLINTConsts.ints)
        }
      }

     //    From PLIC: "meip"
     tile.crossIntIn() :=
       plicOpt .map { _.intnode }
         .getOrElse { meipNode.get }

     //    From PLIC: "seip" (only if vm/supervisor mode is enabled)
     if (tile.tileParams.core.useVM) {
       tile.crossIntIn() :=
         plicOpt .map { _.intnode }
           .getOrElse { NullIntSource() }
     }

    // 3. Local Interrupts ("lip") are required to already be synchronous to the Tile's clock.
    // (they are connected to tile.intInwardNode in a separate trait)

    // 4. Interrupts coming out of the tile are sent to the PLIC,
    //    so might need to be synchronized depending on the Tile's crossing type.
    plicOpt.foreach { plic =>
      FlipRendering { implicit p =>
        plic.intnode :=* tile.crossIntOut()
      }
    }

    // Currently we don't have any interrupts coming from the tile, so we will ignore this for now
//    FlipRendering { implicit p =>
//      plicOutNode.get :=* tile.crossIntOut()
//    }
//    testNode := plicOutNode.get
  }
}

trait HasTilesModuleImp extends freechips.rocketchip.subsystem.HasTilesModuleImp {
  val outer: HasTiles

  val clintint = if(outer.clintNode.isDefined) Some(IO(UInt(INPUT, width = 2))) else None
  if (outer.clintNode.isDefined) {
    outer.clintNode.get.out.foreach { case (int, i) =>
      int(0) := clintint.get(0) // msip
      int(1) := clintint.get(1) // mtip
    }
  }

//  val plicint = if(outer.plicOutNode.isDefined) Some(IO(UInt(OUTPUT, width = 1))) else None
//
//  plicint.get := outer.testNode.in(0)._1(0)
//
//  println(outer.plicOutNode.get.edges.in.map(_.source.num).sum)
//  println(outer.plicOutNode.get.edges.out.map(_.source.num).sum)

//  plicint.get := outer.plicOutNode.get.out.map{ case (int, i) => int.getElements}

//  outer.plicOutNode.get.out.foreach { case (int, i) =>
//    plicint.get := int // msip
//  }
}