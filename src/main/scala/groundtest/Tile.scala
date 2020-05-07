// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package ssithchips.rocketchip.groundtest

import Chisel._
import ssithchips.rocketchip.config._
import ssithchips.rocketchip.diplomacy._
import ssithchips.rocketchip.subsystem._
import ssithchips.rocketchip.interrupts._
import ssithchips.rocketchip.rocket.{DCache, RocketCoreParams}
import ssithchips.rocketchip.tile._
import ssithchips.rocketchip.tilelink._
import scala.collection.mutable.ListBuffer

trait GroundTestTileParams extends TileParams {
  val memStart: BigInt
  val maxRequests: Int
  val numGens: Int

  def build(i: Int, p: Parameters): GroundTestTile
  
  val icache = None
  val btb = None
  val rocc = Nil
  val core = RocketCoreParams(nPMPs = 0) //TODO remove this
  val cached = if(dcache.isDefined) 1 else 0
  val dataScratchpadBytes = 0
}

case object GroundTestTilesKey extends Field[Seq[GroundTestTileParams]]

abstract class GroundTestTile(params: GroundTestTileParams)
                             (implicit p: Parameters)
    extends BaseTile(params, crossing = SynchronousCrossing())(p) {
  val intInwardNode: IntInwardNode = IntIdentityNode()
  val intOutwardNode: IntOutwardNode = IntIdentityNode()
  val slaveNode: TLInwardNode = TLIdentityNode()

  val dcacheOpt = params.dcache.map { dc => LazyModule(new DCache(0)) }

  override lazy val module = new GroundTestTileModuleImp(this)
}

class GroundTestTileModuleImp(outer: GroundTestTile) extends BaseTileModuleImp(outer) {
  val status = IO(new GroundTestStatus)
  val halt_and_catch_fire = None

  outer.dcacheOpt foreach { dcache =>
    val ptw = Module(new DummyPTW(1))
    ptw.io.requestors.head <> dcache.module.io.ptw
  }
}
