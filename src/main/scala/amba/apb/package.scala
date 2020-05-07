// See LICENSE.SiFive for license details.

package ssithchips.rocketchip.amba

import Chisel._
import ssithchips.rocketchip.diplomacy._

package object apb
{
  type APBOutwardNode = OutwardNodeHandle[APBMasterPortParameters, APBSlavePortParameters, APBEdgeParameters, APBBundle]
  type APBInwardNode = InwardNodeHandle[APBMasterPortParameters, APBSlavePortParameters, APBEdgeParameters, APBBundle]
  type APBNode = SimpleNodeHandle[APBMasterPortParameters, APBSlavePortParameters, APBEdgeParameters, APBBundle]
}
