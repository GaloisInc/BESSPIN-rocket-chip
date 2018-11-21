// See LICENSE.SiFive for license details.

package galois.subsystem

import Chisel._
import freechips.rocketchip.devices.debug.TLDebugModule
import freechips.rocketchip.devices.tilelink.{CLINTBase}
import freechips.rocketchip.interrupts._
import freechips.rocketchip.subsystem.{BaseSubsystem}
import freechips.rocketchip.tile.{BaseTile}

trait HasTiles extends freechips.rocketchip.subsystem.HasTiles { this: BaseSubsystem =>

  override protected def connectInterruptsModified(tile: BaseTile, debugOpt: Option[TLDebugModule], clintOpt: Option[CLINTBase]) {
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
//    tile.crossIntIn() := NullIntSource()
//    tile.crossIntIn() := NullIntSource()
    tile.crossIntIn() :=
      clintOpt.map { _.intnode }.get
        //.getOrElse { NullIntSource(sources = CLINTConsts.ints) }


    //    From PLIC: "meip"
    tile.crossIntIn() :=  NullIntSource()

    //    From PLIC: "seip" (only if vm/supervisor mode is enabled)
    if (tile.tileParams.core.useVM) {
      tile.crossIntIn() := NullIntSource()
    }

    // 3. Local Interrupts ("lip") are required to already be synchronous to the Tile's clock.
    // (they are connected to tile.intInwardNode in a seperate trait)

    // 4. Interrupts coming out of the tile are sent to the PLIC,
    //    so might need to be synchronized depending on the Tile's crossing type.
//    plicOpt.foreach { plic =>
//      FlipRendering { implicit p =>
//        plic.intnode :=* tile.crossIntOut()
//      }
//    }
//    FlipRendering { implicit p =>
//      IntSinkNode(IntSinkPortSimple())(ValName("with_no_name")) :=* tile.crossIntOut()
//    }
//
//    IntSinkNode(IntSinkPortSimple())(ValName("no_connection")) := ibus.toPLIC
  }
}