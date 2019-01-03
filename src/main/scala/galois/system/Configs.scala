// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package galois.system

import Chisel._
import freechips.rocketchip.config.Config
import freechips.rocketchip.devices.tilelink.{CLINTKey, CLINTParams, PLICKey, PLICParams}
import galois.devices._
import galois.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.NExtTopInterrupts

class P2Config extends Config(
  new WithoutTLMonitors ++
  new WithNExtTopInterrupts(2) ++
  new WithL1ICacheSets(32) ++
  new WithL1DCacheSets(32) ++
  new WithNBigCores(1) ++
  new WithEdgeDataBits(64) ++
  new WithDTS("galois,rocketchip-p2", Nil) ++
  new BaseConfig
)

class BaseConfig extends Config(
  new WithGFEMemPort() ++
  new WithGFEMMIOPort() ++
  new WithNoSlavePort ++
  new WithTimebase(BigInt(1000000)) ++ // 1 MHz
  new BaseSubsystemConfig()
)

class DefaultConfig extends Config(new P2Config)

class P2FPGAConfig extends Config(
  new WithXilinxJtag ++
  new P2Config
)

class DefaultFPGAConfig extends Config(new P2FPGAConfig)

class WithExtCLINT extends Config((site, here, up) => {
  case CLINTKey => None
  case ExtCLINTKey => Some(CLINTParams())
})

class WithExtPLIC extends Config((site, here, up) => {
  case PLICKey => None
  case ExtPLICKey => Some(PLICParams())
})

class WithoutPLIC extends Config((site, here, up) => {
  case PLICKey => None
  case ExtPLICKey => None
})
