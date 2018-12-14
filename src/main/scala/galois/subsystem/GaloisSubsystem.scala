// See LICENSE.SiFive for license details.

package galois.subsystem

import Chisel._
import chisel3.util.experimental.BoringUtils
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.subsystem.{BaseSubsystem, BaseSubsystemModuleImp, RocketCrossingKey, RocketCrossingParams, RocketTilesKey}
import galois.devices._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.rocket.TracedInstruction
import freechips.rocketchip.util._

trait HasGaloisTiles extends HasTiles
//    with CanHaveExtCLINT
    with CanHavePeripheryPLIC
    with CanHavePeripheryCLINT
//    with CanHaveExtPLIC
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
//    connectInterrupts(rocket, Some(debug), clintOpt, plicOpt, extClintOpt, extPlicOpt)
    connectInterrupts(rocket, Some(debug), clintOpt, plicOpt)

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
  val traceout = IO(Vec(outer.tiles(0).tileParams.core.retireWidth, new TracedInstruction()(outer.tiles(0).p)).asOutput)

  traceout := outer.tiles(0).module.trace.get

  val tvencoder = Module(new TVEncoder()(outer.tiles(0).p))
  tvencoder.io.traceData := (outer.tiles(0).module.trace.get)(0)

  val traceConverter = Module(new TraceConverter()(outer.tiles(0).p))
  traceConverter.io.traceData := (outer.tiles(0).module.trace.get)(0)
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_waddr, Seq(traceConverter.rd))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_ctrl.wfd, Seq(traceConverter.wfd))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_ctrl.wxd, Seq(traceConverter.wxd))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.rf_wdata, Seq(traceConverter.rf_wdata))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.rf_waddr, Seq(traceConverter.rf_waddr))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.ll_wen, Seq(traceConverter.ll_wen))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_wen, Seq(traceConverter.wb_wen))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.rf_wen, Seq(traceConverter.rf_wen))
  BoringUtils.bore(outer.tiles(0).module.core.io.dmem.req.bits.addr, Seq(traceConverter.dmem_addr))
  BoringUtils.bore(outer.tiles(0).module.core.io.dmem.req.bits.data, Seq(traceConverter.dmem_data))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.mem_reg_store, Seq(traceConverter.mem_reg_store))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.mem_reg_load, Seq(traceConverter.mem_reg_load))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_ctrl.mem, Seq(traceConverter.mem))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_ctrl.mem_cmd, Seq(traceConverter.mem_cmd))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.coreMonitorBundle.time, Seq(traceConverter.time))
  BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_set_sboard, Seq(traceConverter.wb_set_sboard))

}
