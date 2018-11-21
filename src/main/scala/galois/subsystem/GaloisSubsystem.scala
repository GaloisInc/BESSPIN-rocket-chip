// See LICENSE.SiFive for license details.

package galois.subsystem

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.subsystem.{BaseSubsystem, BaseSubsystemModuleImp, HasTilesModuleImp, RocketCrossingParams, RocketTilesKey, RocketCrossingKey}
import galois.devices._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

trait HasGaloisTiles extends HasTiles
    with CanHaveExtCLINT
    with CanHavePeripheryPLIC
    with CanHavePeripheryCLINT
    with CanHaveExtPLIC
    with HasPeripheryDebug { this: BaseSubsystem =>
  val module: HasGaloisTilesModuleImp

  protected val rocketTileParams = p(RocketTilesKey)
  private val crossings = perTileOrGlobalSetting(p(RocketCrossingKey), rocketTileParams.size)

  // Make a tile and wire its nodes into the system,
  // according to the specified type of clock crossing.
  // Note that we also inject new nodes into the tile itself,
  // also based on the crossing type.
  val rocketTiles = rocketTileParams.zip(crossings).map { case (tp, crossing) =>
    val rocket = LazyModule(new RocketTile(tp, crossing.crossingType)(augmentedTileParameters(tp)))
      .suggestName(tp.name)

    connectMasterPortsToSBus(rocket, crossing)
    connectSlavePortsToCBus(rocket, crossing)
    connectInterrupts(rocket, Some(debug), clintOpt, plicOpt, extClintOpt, extPlicOpt)

    rocket
  }
}

trait HasGaloisTilesModuleImp extends HasTilesModuleImp
    with HasPeripheryDebugModuleImp {
  val outer: HasGaloisTiles
}

class GaloisSubsystem(implicit p: Parameters) extends BaseSubsystem
    with HasGaloisTiles {
  val tiles = rocketTiles
  override lazy val module = new GaloisSubsystemModuleImp(this)
}

class GaloisSubsystemModuleImp[+L <: GaloisSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
    with HasGaloisTilesModuleImp {
  tile_inputs.zip(outer.hartIdList).foreach { case(wire, i) =>
    wire.clock := clock
    wire.reset := reset
    wire.hartid := UInt(i)
    wire.reset_vector := global_reset_vector
  }
}
