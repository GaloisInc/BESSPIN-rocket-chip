// See LICENSE.SiFive for license details.

package ssithchips.rocketchip.subsystem

import ssithchips.rocketchip.config.{Parameters}
import ssithchips.rocketchip.devices.tilelink._
import ssithchips.rocketchip.diplomacy._
import ssithchips.rocketchip.tilelink._

case class FrontBusParams(
    beatBytes: Int,
    blockBytes: Int,
    zeroDevice: Option[AddressSet] = None,
    errorDevice: Option[DevNullParams] = None)
  extends HasTLBusParams with HasBuiltInDeviceParams

class FrontBus(params: FrontBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "front_bus")
    with CanHaveBuiltInDevices
    with CanAttachTLMasters
    with HasTLXbarPhy {
  attachBuiltInDevices(params)
}
