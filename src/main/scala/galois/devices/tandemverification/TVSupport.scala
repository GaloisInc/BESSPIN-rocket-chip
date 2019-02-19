
// Galois, Inc
//
// Based on reference design and specifications from Bluespec, Inc.
//
// Maintainer: Dylan Hand <dhand@galois.com>

package galois.devices.tandemverification

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.tile.{CoreBundle, XLen}
import freechips.rocketchip.util.DontTouch
import galois.subsystem.{GaloisSubsystem, GaloisSubsystemModuleImp}

// Various support functions, data structures, and parameters

case class TandemVerificationParams(
  debug: Boolean = false,
  dumpTrace: Boolean = false,
  simDelay: Int = 1
)

case object TandemV extends Field[Option[TandemVerificationParams]](None)

class TraceMessage(implicit p: Parameters) extends CoreBundle()(p) {

  val op  = UInt(width = TraceOP.SZ)
  val pc = UInt(width = p(XLen))
  val instr_size = UInt(width = TraceInstrSize.SZ)
  val instr = UInt(width = 32)
  val rd = UInt(width = 5)
  val word1 = UInt(width = p(XLen))
  val word2 = UInt(width = p(XLen))
  val word3 = UInt(width = 64) // Wider than XLen because can contain paddr (in RV32, paddr can be 34 bits)
  val word4 = UInt(width = p(XLen))
}

class TraceVector extends Bundle {
  val vec = Vec(72, UInt(0, 8))
  val count = UInt(7.W)

  // Useful function for TV Encoder. It shifts the contents of vec up by 'shift' bytes and converts to a UInt
  def asShiftedUInt(shift: UInt = 0): UInt = {
    (vec.asUInt() << shift).asUInt()
  }
}

// Traits to add appropriate context and IO to a Subsystem
trait CanHaveTandemVerification { this: GaloisSubsystem =>
  implicit val p : Parameters
}

trait HasTVBundle {
  implicit val p : Parameters
  val traceout: DecoupledIO[TraceVector]

  def connectTVConsumer() {
    val tvconsumer = Module(new TVConsumer(p(TandemV).getOrElse(new TandemVerificationParams)))
    tvconsumer.io.traceout <> traceout
  }
}

// Adds the tv encoder and connects it to an existing tiletap
trait CanHaveTandemVerificationModuleImp extends LazyModuleImp with HasTVBundle {
  this: GaloisSubsystemModuleImp[GaloisSubsystem] =>
  val outer: CanHaveTandemVerification
  val traceout = IO(Decoupled(new TraceVector))

  tvencoder.map(traceout <> _.io.encodedOutput)
}

// Useful block for debugging inside the rocket testbench. It can implement arbitrarily long delays while processing
// a trace vector. It also handles printing the trace dump that can be postprocessed later
class TVConsumer(params: TandemVerificationParams) extends Module with DontTouch {
  val io = IO(new Bundle {
    val traceout = Decoupled(new TraceVector).flip()
  })

  val counter = RegInit(0.U(32.W))
  val runCounter = RegInit(Bool(false))

  when(io.traceout.valid & !io.traceout.ready) {
    runCounter := Bool(true)
  }.elsewhen(counter =/= 0.U) {
    runCounter := Bool(false)
  }

  when (io.traceout.fire()) {
    if (params.debug) printf("[DUT] Vector Accepted!\n")
    // Format is the number of valid bytes followed by the byte array in hex format. This array will be packed right to left
    // so the post-processing software will likely have to byte-reverse the string
    if (params.dumpTrace) printf("[DUT] [HEX] [%d] %x\n", io.traceout.data.bits.count, io.traceout.data.bits.vec.asUInt())
  }

  when (runCounter || counter =/= 0.U) {
    counter := counter + 1.U
    if (params.debug) printf("[DUT] Count = %d\n", counter)
    io.traceout.ready := Bool(false)
    when (counter === params.simDelay) {
      io.traceout.ready := Bool(true)
      counter := 0.U
      if (params.debug) printf("[DUT] MSG COMPLETE\n")
    }
  }.otherwise {
    io.traceout.ready := Bool(false)
  }

  dontTouch(runCounter)
  dontTouch(counter)
}
