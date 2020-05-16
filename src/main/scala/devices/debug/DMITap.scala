package freechips.rocketchip.devices.debug

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.HasBlackBoxResource

class DMITap(implicit val p: Parameters) extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle() {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val dmi_in = Flipped(new ClockedDMIIO())
    val dmi_out = new ClockedDMIIO()
  })
  addResource("/vsrc/DMITap.v")
}
