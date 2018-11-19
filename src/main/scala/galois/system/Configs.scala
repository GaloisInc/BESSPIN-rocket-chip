// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package galois.system

import Chisel._
import freechips.rocketchip.config.Config
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._


// class WithJtagDTMSystem extends freechips.rocketchip.subsystem.WithJtagDTM

class P1Config extends Config(
  new WithRV32 ++ 
  new WithoutFPU ++
  new WithNExtTopInterrupts(16) ++
  new WithL1ICacheSets(16) ++
  new WithL1DCacheSets(16) ++
  new BaseConfig
)

class BaseConfig extends Config(
  new WithDefaultMemPort() ++
  new WithDefaultMMIOPort() ++
  new WithNoSlavePort ++
  new WithTimebase(BigInt(1000000)) ++ // 1 MHz
  new WithDTS("galois,rocketchip-p1", Nil) ++
//  new WithJtagDTMSystem ++
  new BaseSubsystemConfig()
)

class DefaultConfig extends Config(new P1Config)

class P1FPGAConfig extends Config(new P1Config)

class DefaultFPGAConfig extends Config(new P1FPGAConfig)
