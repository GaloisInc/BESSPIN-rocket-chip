// See LICENSE.SiFive for license details.

package ssithchips.rocketchip.amba

import Chisel._
import ssithchips.rocketchip.diplomacy._

package object ahb
{
  type AHBOutwardNode = OutwardNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBBundle]
  type AHBInwardNode = InwardNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBBundle]
  type AHBNode = SimpleNodeHandle[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBBundle]
}
