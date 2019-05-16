package galois.devices

import Chisel._
import chisel3.core.{Input, IntParam, Output}
import chisel3.util.HasBlackBoxResource
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.jtag._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._

trait HasPeripheryDebugWithTapModuleImp extends LazyModuleImp {
  val outer: HasPeripheryDebug

  val debug = IO(new DebugIO)

  require(!(debug.clockeddmi.isDefined && debug.systemjtag.isDefined),
    "You cannot have both DMI and JTAG interface in HasPeripheryDebugWithTapModuleImp")

  debug.clockeddmi.foreach { dbg => outer.debug.module.io.dmi <> dbg }

  val tap = Module(new DMITap)
  val dtm = debug.systemjtag.map { instantiateJtagDTM(_) }


  debug.ndreset  := outer.debug.module.io.ctrl.ndreset
  debug.dmactive := outer.debug.module.io.ctrl.dmactive

  // TODO in inheriting traits: Set this to something meaningful, e.g. "component is in reset or powered down"
  outer.debug.module.io.ctrl.debugUnavail.foreach { _ := Bool(false) }

  def instantiateJtagDTM(sj: SystemJTAGIO): DebugTransportModuleJTAG = {

    val dtm = Module(new DebugTransportModuleJTAG(p(DebugModuleParams).nDMIAddrSize, p(JtagDTMKey)))
    dtm.io.jtag <> sj.jtag

    dtm.clock          := sj.jtag.TCK
    dtm.io.jtag_reset  := sj.reset
    dtm.io.jtag_mfr_id := sj.mfr_id
    dtm.reset          := dtm.io.fsmReset

    outer.debug.module.io.dmi <> tap.io.dmi_out
    tap.io.dmi_in.dmi <> dtm.io.dmi
    tap.io.dmi_in.dmiClock := sj.jtag.TCK

    val psd = debug.psd.getOrElse(Wire(new PSDTestMode).fromBits(0.U))
    outer.debug.module.io.psd <> psd
    tap.io.dmi_in.dmiReset := ResetCatchAndSync(sj.jtag.TCK, sj.reset, "dmiResetCatch", psd)
    dtm
  }
}

class DMITap(implicit val p: Parameters) extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle() {
    val dmi_in = new ClockedDMIIO().flip()
    val dmi_out = new ClockedDMIIO()
  })
  setResource("/vsrc/DMITap.v")
}