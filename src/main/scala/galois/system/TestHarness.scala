// See LICENSE.SiFive for license details.

package galois.system

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.debug.Debug
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.util.DontTouch

class TestHarness()(implicit p: Parameters) extends Module with DontTouch {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }

  val dut = Module(LazyModule(new P1System).module)
  dut.reset := reset | dut.debug.ndreset


  dut.dontTouchPorts()

  dut.connectTVConsumer()

  dut.tieOffInterrupts()
  dut.connectSimAXIMem()
  dut.connectSimAXIMMIO()
  Debug.connectDebug(dut.debug, clock, reset, io.success)
}
