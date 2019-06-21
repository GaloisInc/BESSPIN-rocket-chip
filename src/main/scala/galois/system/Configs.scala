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
  new WithGFEMemPort() ++
  new WithGFEMMIOPort() ++
  new WithoutTLMonitors ++
  new WithNExtTopInterrupts(16) ++
  new WithL1ICacheSets(32) ++
  new WithL1DCacheSets(32) ++
  new WithNBigCores(1) ++
  new WithEdgeDataBits(64) ++
  new WithDTS("galois,rocketchip-p2", Nil) ++
  new BaseConfig
)

class P1Config extends Config(
  new WithRV32 ++ 
  new WithoutFPU ++
  new WithGFEMemPort() ++
  new WithGFEMMIOPort() ++
  new WithoutTLMonitors ++
  new WithL1ICacheSets(64) ++
  new WithL1DCacheSets(64) ++
  new WithNSmallCores(1) ++
  new WithEdgeDataBits(64) ++
  new WithNExtTopInterrupts(16) ++
  new WithDTS("galois,rocketchip-p1", Nil) ++
  new BaseConfig
)

class P1TVConfig extends Config(
  new WithTandemVerification(false, false) ++
  new P1Config
)

class P2TVConfig extends Config(
  new WithTandemVerification() ++
  new P2Config
)

class BaseConfig extends Config(
  new WithGFECLINT ++
  new WithDefaultMemPort ++
  new WithDefaultMMIOPort ++
  new WithNoSlavePort ++
  new WithTimebase(BigInt(100000000)) ++ // 100 MHz - Sets RTC tick to match global clock rate
  new BaseSubsystemConfig()
)

class DefaultConfig extends Config(new P2Config)

class P2FPGAConfig extends Config(
  new WithXilinxJtag ++
  new P2Config
)

class P2TVFPGAConfig extends Config(
  new WithXilinxJtag ++
  new WithGFEMemPort() ++
  new WithGFEMMIOPort() ++
  new P2TVConfig
)

class P1FPGAConfig extends Config(
  new WithXilinxJtag ++
  new WithGFEMemPort() ++
  new WithGFEMMIOPort() ++
  new P1Config
)

class P1TVFPGAConfig extends Config(
  new WithXilinxJtag ++
  new P1TVConfig
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
