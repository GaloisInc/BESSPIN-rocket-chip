// See LICENSE.SiFive for license details.

package galois.system

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util.DontTouch

/** P1 top with periphery devices and ports, and a Rocket subsystem */
class P1System(implicit p: Parameters) extends RocketSubsystem
    with CanHaveMasterAXI4MemPort
    with CanHaveMasterAXI4MMIOPort
    with HasExtPLIC
{
  override lazy val module = new P1SystemModuleImp(this)

  // The sbus masters the cbus; here we convert TL-UH -> TL-UL
  sbus.crossToBus(cbus, NoCrossing)

  // The cbus masters the pbus; which might be clocked slower
  cbus.crossToBus(pbus, SynchronousCrossing())

  // The fbus masters the sbus; both are TL-UH or TL-C
  FlipRendering { implicit p =>
    sbus.crossFromBus(fbus, SynchronousCrossing())
  }

  // The sbus masters the mbus; here we convert TL-C -> TL-UH
  private val BankedL2Params(nBanks, coherenceManager) = p(BankedL2Key)
  private val (in, out, halt) = coherenceManager(this)
  if (nBanks != 0) {
    sbus.coupleTo("coherence_manager") { in :*= _ }
    mbus.coupleFrom("coherence_manager") { _ :=* BankBinder(mbus.blockBytes * (nBanks-1)) :*= out }
  }
}

class P1SystemModuleImp[+L <: P1System](_outer: L) extends RocketSubsystemModuleImp(_outer)
    with HasExtCLINTModuleImp
    with CanHaveMasterAXI4MemPortModuleImp
    with CanHaveMasterAXI4MMIOPortModuleImp
    with HasExtPLICModuleImp
    with DontTouch
