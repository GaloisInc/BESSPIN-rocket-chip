// See LICENSE.SiFive for license details.

package galois.system

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import galois.subsystem._
import galois.devices.tandemverification._
import freechips.rocketchip.subsystem.{CanHaveMasterAXI4MemPort, CanHaveMasterAXI4MMIOPort, BankedL2Params, NExtTopInterrupts,
  BankedL2Key, HasRTCModuleImp, CanHaveMasterAXI4MMIOPortModuleImp, HasAsyncExtInterrupts, HasExtInterruptsModuleImp}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util.DontTouch
import galois.devices.ExtPLICKey

/** Top with periphery devices and ports, and a Rocket subsystem */
class GaloisSystem(implicit p: Parameters) extends GaloisSubsystem
  with HasAsyncExtInterrupts
  with CanHaveTandemVerification
    with CanHaveMasterAXI4MemPort
    with CanHaveMasterAXI4MMIOPort
{
  override lazy val module = new GaloisSystemModuleImp(this)

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
  if (p(PLICKey).isEmpty & p(ExtPLICKey).isEmpty) {
    require(p(NExtTopInterrupts) == 1, "Must include one single external interrupt when not instantiating the PLIC")
  }
}

class GaloisSystemModuleImp[+L <: GaloisSystem](_outer: L) extends GaloisSubsystemModuleImp(_outer)
    with HasRTCModuleImp
    with HasGaloisGFEResetVectorImp
    with CanHaveGFEMasterAXI4MemPortModuleImp
    with CanHaveMasterAXI4MMIOPortModuleImp
    with HasExtInterruptsModuleImp
    with CanHaveTandemVerificationModuleImp
    with DontTouch
