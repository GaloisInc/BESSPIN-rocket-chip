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
import galois.devices.tandemverification._
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


  val tvencoder = p(TandemV).map { params =>
    val tve = Module(new TVEncoder(params)(outer.tiles(0).p))
    tve
  }

  val traceConverter = p(TandemV).map { params =>
    val tc = Module(new TVTileTap(params)(outer.tiles(0).p))
    tc
  }

  if (tvencoder.isDefined && traceConverter.isDefined) {
    require(outer.tiles.length == 1, "Tandem verification is currently only supported on single tile systems!")

    traceConverter.get.io.traceData := (outer.tiles(0).module.trace.get) (0)
    tvencoder.get.io.traceData <> traceConverter.get.io.traceMsg

    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_waddr, Seq(traceConverter.get.rd))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_ctrl.wfd, Seq(traceConverter.get.wfd))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_ctrl.wxd, Seq(traceConverter.get.wxd))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.rf_wdata, Seq(traceConverter.get.rf_wdata))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.rf_waddr, Seq(traceConverter.get.rf_waddr))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.ll_wen, Seq(traceConverter.get.ll_wen))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_wen, Seq(traceConverter.get.wb_wen))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.rf_wen, Seq(traceConverter.get.rf_wen))
    BoringUtils.bore(outer.tiles(0).module.core.io.dmem.req.bits.addr, Seq(traceConverter.get.dmem_addr))
    BoringUtils.bore(outer.tiles(0).module.core.io.dmem.req.bits.data, Seq(traceConverter.get.dmem_data))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.mem_reg_store, Seq(traceConverter.get.mem_reg_store))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.mem_reg_load, Seq(traceConverter.get.mem_reg_load))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_ctrl.mem, Seq(traceConverter.get.mem))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_ctrl.mem_cmd, Seq(traceConverter.get.mem_cmd))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_ctrl.branch, Seq(traceConverter.get.branch))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_ctrl.csr, Seq(traceConverter.get.wb_csr))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.mem_npc, Seq(traceConverter.get.mem_npc))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.insn_ret, Seq(traceConverter.get.csr_insn_ret))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.read_mstatus, Seq(traceConverter.get.csr_mstatus))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.io.rw.addr, Seq(traceConverter.get.csr_addr))
    // Can't seem to use mip directly...
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.mip_update, Seq(traceConverter.get.mip_update))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.mip_value, Seq(traceConverter.get.mipval))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.io.rw.wdata, Seq(traceConverter.get.csr_wdata))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.coreMonitorBundle.time, Seq(traceConverter.get.time))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.wb_set_sboard, Seq(traceConverter.get.wb_set_sboard))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.exception, Seq(traceConverter.get.exception))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.epc, Seq(traceConverter.get.epc))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.cause, Seq(traceConverter.get.cause))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.io.tval, Seq(traceConverter.get.tval))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.reg_mstatus.prv, Seq(traceConverter.get.epriv))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.trap_mstatus_uint, Seq(traceConverter.get.trap_mstatus))
    BoringUtils.bore(outer.tiles(0).module.core.rocketImpl.csr.trapToDebug, Seq(traceConverter.get.trapToDebug))
    BoringUtils.bore(tvencoder.get.tv_stall, Seq(outer.tiles(0).module.core.rocketImpl.tv_block))

    // FPU
    BoringUtils.bore(outer.tiles(0).module.fpuOpt.get.fpuImpl.load_wb, Seq(traceConverter.get.fpu_load_wb))
    BoringUtils.bore(outer.tiles(0).module.fpuOpt.get.fpuImpl.load_wb_tag, Seq(traceConverter.get.fpu_tag))
    BoringUtils.bore(outer.tiles(0).module.fpuOpt.get.fpuImpl.load_wb_data, Seq(traceConverter.get.fpu_data))
  }

}
