// See LICENSE.SiFive for license details.

package ssithchips.rocketchip.subsystem

import Chisel._
import ssithchips.rocketchip.config.{Parameters}
import ssithchips.rocketchip.devices.tilelink._
import ssithchips.rocketchip.diplomacy._
import ssithchips.rocketchip.tilelink._
import ssithchips.rocketchip.util._

case class SystemBusParams(
    beatBytes: Int,
    blockBytes: Int,
    policy: TLArbiter.Policy = TLArbiter.roundRobin,
    zeroDevice: Option[AddressSet] = None,
    errorDevice: Option[DevNullParams] = None)
  extends HasTLBusParams with HasBuiltInDeviceParams

class SystemBus(params: SystemBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "system_bus")
    with CanHaveBuiltInDevices
    with CanAttachTLSlaves
    with CanAttachTLMasters
    with HasTLXbarPhy {
  attachBuiltInDevices(params)

  def fromTile
      (name: Option[String], buffer: BufferParams = BufferParams.none, cork: Option[Boolean] = None)
      (gen: => TLOutwardNode): NoHandle = {
    from("tile" named name) {
      inwardNode :=* TLBuffer(buffer) :=* TLFIFOFixer(TLFIFOFixer.allUncacheable) :=* gen
    }
  }
}
