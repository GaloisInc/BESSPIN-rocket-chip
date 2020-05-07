// See LICENSE.SiFive for license details.

package ssithchips.rocketchip.groundtest

import Chisel._

import ssithchips.rocketchip.config.Parameters
import ssithchips.rocketchip.diplomacy.LazyModule

class TestHarness(implicit p: Parameters) extends Module {
  val io = new Bundle { val success = Bool(OUTPUT) }
  val dut = Module(LazyModule(new GroundTestSubsystem).module)
  io.success := dut.success
  dut.connectSimAXIMem()
}
