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


class WithJtagDTMSystem extends galois.subsystem.WithJtagDTM

class P1Config extends Config(
  new WithRV32 ++ 
  new WithoutFPU ++
  new WithoutTLMonitors ++
  new WithoutPLIC ++
  new WithL1ICacheSets(64) ++
  new WithL1DCacheSets(64) ++
  new WithNSmallCores(1) ++
  new WithEdgeDataBits(64) ++
  new WithDTS("galois,rocketchip-p1", Nil) ++
  new WithBootROMFile("./bootrom/bootrom.img") ++
  new BaseConfig
)

class BaseConfig extends Config(
  new WithGFEMemPort() ++
  new WithGFEMMIOPort() ++
  new WithNoSlavePort ++
  new WithTimebase(BigInt(1000000)) ++ // 1 MHz
  new BaseSubsystemConfig()
)

class DefaultConfig extends Config(new P1Config)

class P1FPGAConfig extends Config(
  new WithJtagDTM ++
  new P1Config
)

class DefaultFPGAConfig extends Config(new P1FPGAConfig)

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
