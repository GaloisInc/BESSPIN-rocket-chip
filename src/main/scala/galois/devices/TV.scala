
package galois.devices

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile.{CoreBundle, CoreModule, HasCoreParameters, XLen}

object TraceOP {
  def SZ = 5
  def trace_reset = UInt(0, SZ)
  def trace_gpr_write = UInt(1, SZ)
  def trace_fpr_write = UInt(2, SZ)
  def trace_csr_write = UInt(3, SZ)
  def trace_mem_write = UInt(4, SZ)
  def trace_other = UInt(5, SZ)
  def trace_i_rd = UInt(6, SZ)
  def trace_f_rd = UInt(7, SZ)
  def trace_i_load = UInt(8, SZ)
  def trace_f_load = UInt(9, SZ)
  def trace_store = UInt(10, SZ)
  def trace_amo = UInt(11, SZ)
  def trace_trap = UInt(12, SZ)
  def trace_ret = UInt(13, SZ)
  def trace_csrrx = UInt(14, SZ)
  def trace_intr = UInt(15, SZ)
}

object TraceInstrSize {
  def SZ = 1
  def isize_16bit = UInt(0, SZ)
  def isize_32bit = UInt(1, SZ)
}

class TraceMessage(implicit p: Parameters) extends CoreBundle {

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


class TVEncoderIO()(implicit p: Parameters) extends Bundle {
  val traceData = (new TracedInstruction).asInput
  val encodedOutput = Bool(OUTPUT)
}

class TVEncoder()(implicit p: Parameters) extends Module with DontTouch {
  val io = new TVEncoderIO()

  val reg = Reg(new TracedInstruction)

  when (io.traceData.valid) {
    reg := io.traceData
    io.encodedOutput := 1
  }
  dontTouch(reg)
}

class TraceConverterIO()(implicit p: Parameters) extends Bundle {
  val traceData = (new TracedInstruction).asInput
  val traceMsg = (new TraceMessage).asOutput
}

class TraceConverter()(implicit p: Parameters) extends Module {
  val io = new TraceConverterIO()

  val t = io.traceData
  val rd = Wire(UInt(width = 5))
  val wfd = Wire(Bool())
  val wxd = Wire(Bool())
  val rf_wdata = Wire(UInt(width = p(XLen)))
  val ll_wen = Wire(Bool())
  val rf_waddr = Wire(UInt(width = 5))
  val wb_set_sboard = Wire(Bool())
  val wb_wen = Wire(Bool())
  val rf_wen = Wire(Bool())
  val has_data = wb_wen && !wb_set_sboard
  val time = Wire(UInt(width = 32))
  val dmem_addr = Wire(UInt(width = 32))
  val stored_addr = Reg(next = Reg(next = dmem_addr))
  val dmem_data = Wire(UInt(width = p(XLen)))
  val stored_data = Reg(next = Reg(next = dmem_data))
  val mem_reg_store = Wire(Bool())
  val mem_reg_load = Wire(Bool())
  val mem = Wire(Bool())
  val mem_cmd = Wire(Bits(width = 5))
  val isAMOInsn = mem && (isAMO(mem_cmd) || mem_cmd.isOneOf(M_XLR, M_XSC))
  val branch = Wire(Bool())
  val mem_npc = Wire(UInt(width = p(XLen)))
  val npc = Reg(next = mem_npc)
  val isLoad = Reg(next = mem_reg_load)
  val isStore = Reg(next = mem_reg_store)

  def mkTrace_I_RD(msg: TraceMessage, pc: UInt, isize: UInt, instruction: UInt, rd: UInt, rdvalue: UInt) = {
    //val msg = new TraceMessage()
    msg.op := TraceOP.trace_i_rd
    msg.pc := pc
    msg.instr_size := TraceInstrSize.isize_32bit
    msg.instr := instruction
    msg.rd := rd
    msg.word1 := rdvalue
  }

  def mkTrace_I_LOAD(msg: TraceMessage, pc: UInt, isize: UInt, instruction: UInt, rd: UInt, rdvalue: UInt, eaddr: UInt) = {
    mkTrace_I_RD(msg, pc, isize, instruction, rd, rdvalue)
    msg.op := TraceOP.trace_i_load
    msg.word3 := eaddr
  }

  def mkTrace_I(msg: TraceMessage, isLoad: Bool, pc: UInt, isize: UInt, instruction: UInt, rd: UInt, rdvalue: UInt, eaddr: UInt): Unit = {
    when (isLoad) {
      mkTrace_I_LOAD(msg, pc, isize, instruction, rd, rdvalue, eaddr)
    }.otherwise {
      mkTrace_I_RD(msg, pc, isize, instruction, rd, rdvalue)
    }
  }

  def mkTrace_AMO(msg: TraceMessage, pc: UInt, isize: UInt, instruction: UInt, rd: UInt, rdvalue: UInt, data: UInt, eaddr: UInt) = {
    msg.op := TraceOP.trace_amo
    msg.pc := pc
    msg.instr_size := TraceInstrSize.isize_32bit
    msg.instr := instruction
    msg.rd := rd
    msg.word1 := rdvalue
    msg.word2 := data
    msg.word3 := eaddr
  }

  def mkTrace_STORE(msg: TraceMessage, pc: UInt, isize: UInt, instruction: UInt, data: UInt, eaddr: UInt) = {
    msg.op := TraceOP.trace_store
    msg.pc := pc
    msg.instr_size := TraceInstrSize.isize_32bit
    msg.instr := instruction
    msg.word2 := data
    msg.word3 := eaddr
  }

  def mkTrace_OTHER(msg: TraceMessage, pc: UInt, isize: UInt, instruction: UInt) = {
    msg.op := TraceOP.trace_other
    msg.pc := pc
    msg.instr_size := TraceInstrSize.isize_32bit
    msg.instr := instruction
  }

  def mkTrace_RESET(msg: TraceMessage) = {
    msg.op := TraceOP.trace_reset
    printf("Reset Trace\n")
  }

  /*
    Repurposing the commit log code to create trace messages.

    has_data indicates when the instruction is committed but the data is not yet available for writeback.
    This code will create a trace object, but hold off on sending it until the writeback value is available.
    The wb value is available when rf_wen == 1 and rd == saved_rd.

    This assumes only 1 write can be outstanding at a time (not true?) and that no other instructions are committed
    until the writeback value is available.
   */

  val storedMsg = Reg(new TraceMessage)
  val isMsgStored = Reg(Bool())
  val inReset = Reg(init = Bool(true))
  when (inReset) {
    inReset := false
    mkTrace_RESET(io.traceMsg)
  }

  when (t.valid && !t.exception) {
    isMsgStored := 0
    when (wfd) {
      printf ("C0: %d : %d 0x%x (0x%x) f%d p%d 0xXXXXXXXXXXXXXXXX\n", time, t.priv, t.iaddr, t.insn, rd, rd+UInt(32))
    }
      .elsewhen (wxd && rd =/= UInt(0) && has_data) {
        mkTrace_I(io.traceMsg, isLoad, t.iaddr, UInt(1), t.insn, rd, rf_wdata, stored_addr)
        printf ("C0: %d : %d 0x%x (0x%x) x%d 0x%x addr = 0x%x\n", time, t.priv, t.iaddr, t.insn, rd, rf_wdata, stored_addr)
        assert(branch === false)
        when (isAMOInsn) {
          mkTrace_AMO(io.traceMsg, t.iaddr, UInt(1), t.insn, rd, rf_wdata, stored_data, stored_addr)
        }
      }
      .elsewhen (wxd && rd =/= UInt(0) && !has_data) {
        mkTrace_I(storedMsg, isLoad, t.iaddr, UInt(1), t.insn, rd, rf_wdata, stored_addr)
        assert(branch === false)
        isMsgStored := 1
        printf("C0: %d : Stored Message for rd = %d\n", time, rd)
        printf ("C0: %d : %d 0x%x (0x%x) x%d p%d 0xXXXXXXXXXXXXXXXX addr = 0x%x\n", time, t.priv, t.iaddr, t.insn, rd, rd, stored_addr)
      }
      .otherwise {
        printf ("C0: %d : %d 0x%x (0x%x)\n", time, t.priv, t.iaddr, t.insn)
        when (branch) {
          printf ("C0: %d : Branch! npc = 0x%x\n", time, npc)
          mkTrace_OTHER(io.traceMsg, t.iaddr, UInt(1), t.insn)
        }
        when (isLoad) { printf("Is Load!\n") }
        when (isStore) { printf("Is Store! 0x%x @ 0x%x\n", stored_data, stored_addr)
          mkTrace_STORE(io.traceMsg, t.iaddr, UInt(1), t.insn, stored_data, stored_addr) }
      }
  }

  when (isMsgStored && rf_wen && rd === storedMsg.rd) {
    isMsgStored := 0
    io.traceMsg := storedMsg
    io.traceMsg.word1 := rf_wdata
    assert(branch === false)
    printf("C0: %d : Stored 0x%x into reg %d from 0x%x\n", time, rf_wdata, storedMsg.rd, stored_addr)
  }

  when (ll_wen && rf_waddr =/= UInt(0)) {
    printf ("C0: %d : x%d p%d 0x%x\n", time, rf_waddr, rf_waddr, rf_wdata)
    assert(branch === false)
    when (isLoad) { printf("Is Load!\n") }
    when (isStore) { printf("Is Store 2! 0x%x @ 0x%x\n", stored_data, stored_addr)
      mkTrace_STORE(io.traceMsg, t.iaddr, UInt(1), t.insn, stored_data, stored_addr) }
  }

}
