
// Galois, Inc
//
// Based on reference design and specifications from Bluespec, Inc.
//
// Maintainer: Dylan Hand <dhand@galois.com>

package galois.devices.tandemverification

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile.{CoreParams, HasCoreParameters, TileKey, XLen}

class TileTapIO()(implicit p: Parameters) extends Bundle {
  val traceData = (new TracedInstruction).asInput
  val traceMsg = Decoupled(new TraceMessage)
}

class TVTileTap(params: TandemVerificationParams)(implicit p: Parameters) extends Module() {
  val io = new TileTapIO()

  val t             = io.traceData
  val rd            = Wire(UInt(width = 5))
  val wfd           = Wire(Bool())
  val wxd           = Wire(Bool())
  val rf_wdata      = Wire(UInt(width = p(XLen)))
  val ll_wen        = Wire(Bool())
  val rf_waddr      = Wire(UInt(width = 5))
  val wb_set_sboard = Wire(Bool())
  val wb_wen        = Wire(Bool())
  val rf_wen        = Wire(Bool())
  val has_data      = wb_wen && !wb_set_sboard
  val time          = Wire(UInt(width = 32))
  val dmem_addr     = Wire(UInt(width = if (p(XLen) == 64) 64 else 32))
  val stored_addr   = Reg(next = Reg(next = dmem_addr))
  val dmem_data     = Wire(UInt(width = p(XLen)))
  val stored_data   = Reg(next = Reg(next = dmem_data))
  val mem_reg_store = Wire(Bool())
  val mem_reg_load  = Wire(Bool())
  val mem           = Wire(Bool())
  val mem_cmd       = Wire(Bits(width = 5))
  val isAMOInsn     = mem && (isAMO(mem_cmd) || mem_cmd.isOneOf(M_XLR, M_XSC))
  val branch        = Wire(Bool())
  val mem_npc       = Wire(UInt(width = p(XLen)))
  val npc           = Reg(next = mem_npc)
  val wb_csr        = Wire(Bits(width = CSR.SZ))
  val csr_insn_ret  = Wire(Bool())
  val csr_mstatus   = Wire(UInt(width = p(XLen)))
  val csr_addr      = Wire(UInt(width = CSR.ADDRSZ))
  val csr_wdata     = Wire(UInt(width = p(XLen)))
  val isLoad        = Reg(next = mem_reg_load)
  val isStore       = Reg(next = mem_reg_store)
  val mipval        = Wire(UInt(width = p(XLen)))
  val mip_update    = Wire(Bool())
  val csr_wen       = Wire(Bool())
  val csr_trace_wdata = Wire(UInt(width = p(XLen)))
  val csr_trace_waddr = Wire(UInt(width = CSR.ADDRSZ))

  // For Trap
  val exception    = Wire(Bool())
  val epc          = Wire(UInt(width = p(XLen)))
  val cause        = Wire(UInt(width = p(XLen)))
  val tval         = Wire(UInt(width = p(XLen)))
  val epriv        = Wire(UInt(width = p(XLen)))
  val trapToDebug  = Wire(Bool())
  val trap_mstatus = Wire(UInt(width = p(XLen)))

  val isCSRRX = Wire(Bool())

  isCSRRX := wb_csr.isOneOf(CSR.W, CSR.S, CSR.C)

  // For Compressed Instructions
  val isCompressed = Wire(Bool())

  isCompressed := t.insn(1, 0).andR === 0.U(1.W)

  // For FPU
  val fLen        = p(TileKey).core.fpu.map(_.fLen).getOrElse(0)
  val fpu_load_wb = Wire(Bool())
  val fpu_tag     = Wire(UInt(5))
  val fpu_data    = Wire(UInt(width = fLen))

  when(t.interrupt) {
    if (params.debug) printf("[TV] [Tile] %d Interrupt occurred\n", time)
  }

  /*
    Repurposing the commit log code to create trace messages.

    has_data indicates when the instruction is committed but the data is not yet available for writeback.
    This code will create a trace object, but hold off on sending it until the writeback value is available.
    The wb value is available when rf_wen == 1 and rd == saved_rd.

    This assumes only 1 write can be outstanding at a time (not true?) and that no other instructions are committed
    until the writeback value is available.
   */

  val storedMsg   = Reg(new TraceMessage)
  val isMsgStored = RegInit(false.B)
  val inReset     = Reg(init = Bool(true))

  val isMsgReady     = Wire(Bool())
  val storedMsgIsCSR = Wire(Bool())
  val storedMsgReady = Wire(Bool())
  val isStoreReady   = Wire(Bool())

  io.traceMsg.valid := isMsgReady || storedMsgReady || isStoreReady || inReset // mip_update

  when(inReset) {
    inReset := false
    TVFunctions.generate_tm_reset(io.traceMsg.bits)(params)
  }

  //  when (mip_update) {
  //    if (params.debug) printf("[TV] [Tile] %d MIP has changed! mip = 0x%x\n", time, mipval)
  //    // TODO: Replace with correct MIP address
  //    TVFunctions.generate_tm_csrw(io.traceMsg.bits, UInt(36), mipval)(params)
  //  }

  val isNop = Wire(Bool())
  isNop := t.insn === 0x00000013

  storedMsgIsCSR := storedMsg.op === TraceOP.trace_csr_write

  printf("[TV] [Tile] C0: %d | npc = 0x%x | mem_npc = 0x%x\n", time, npc, mem_npc)

  when(fpu_load_wb) {
    if (params.debug) printf("[TV] [Tile] [FP] [%d] Addr = 0x%x | Data = 0x%x!\n", time, fpu_tag, fpu_data)
  }

  when(t.valid && !t.exception) {
    // Make sure we don't clear out a previously stored message if a nop is committed
    when(isMsgStored) {
      when(isNop == false) {
        if (params.debug) printf("[TV] [Tile] [WARN] New instruction committed before previously stored instruction sent to TraceEncoder. Check instruction stream!\n")
      }
    }.otherwise {
      isMsgStored := 0
    }

    isMsgReady := false
    when(wfd) {
      if (params.debug) printf("[TV] [Tile] C0: %d : %d 0x%x (0x%x) f%d p%d 0xXXXXXXXXXXXXXXXX\n", time, t.priv, t.iaddr, t.insn, rd, rd + UInt(32))
      if (params.debug) printf("[TV] [Tile] Floating point instruction\n")
    }
      .elsewhen(wxd && rd =/= UInt(0) && has_data) {
        TVFunctions.generate_tm_i(io.traceMsg.bits, isLoad, npc, isCompressed, t.insn, rd, rf_wdata, stored_addr)(params)
        isMsgReady := true
        if (params.debug) printf("[TV] [Tile] C0: %d : %d 0x%x (0x%x) x%d 0x%x addr = 0x%x\n", time, t.priv, t.iaddr, t.insn, rd, rf_wdata, stored_addr)
        assert(branch === false)
        when(isCSRRX) {
          if (params.debug) printf("[TV] [Tile] C0: %d : CSRRX Instruction: CSR Addr = 0x%x | CSR Data = 0x%x\n", time, csr_addr, csr_wdata)
          TVFunctions.generate_tm_csrrx(io.traceMsg.bits, npc, isCompressed, t.insn, rd, rf_wdata, Bool(true), csr_addr, csr_wdata)
        }.elsewhen(isAMOInsn) {
          TVFunctions.generate_tm_amo(io.traceMsg.bits, npc, isCompressed, t.insn, rd, rf_wdata, stored_data, stored_addr)
        }.otherwise {
          TVFunctions.generate_tm_i_rd(io.traceMsg.bits, npc, isCompressed, t.insn, rd, rf_wdata)
        }
      }
      .elsewhen(wxd && rd =/= UInt(0) && !has_data) {
        TVFunctions.generate_tm_i(storedMsg, isLoad, npc, isCompressed, t.insn, rd, rf_wdata, stored_addr)(params)
        assert(branch === false)
        isMsgStored := 1
        if (params.debug) printf("[TV] [Tile] C0: %d : Stored Message for rd = %d\n", time, rd)
        if (params.debug) printf("[TV] [Tile] C0: %d : %d 0x%x (0x%x) x%d p%d 0xXXXXXXXXXXXXXXXX addr = 0x%x\n", time, t.priv, t.iaddr, t.insn, rd, rd, stored_addr)
      }
      .otherwise {
        if (params.debug) printf("[TV] [Tile] C0: %d : %d 0x%x (0x%x)\n", time, t.priv, t.iaddr, t.insn)
        when(branch) {
          if (params.debug) printf("[TV] [Tile] C0: %d : Branch npc = 0x%x\n", time, npc)
          TVFunctions.generate_tm_other(io.traceMsg.bits, npc, isCompressed, t.insn)(params)
          isMsgReady := true
        }.elsewhen(csr_wen) {
          if (params.debug) printf("[TV] [Tile] C0: %d : CSR Write Enabled\n", time)
          TVFunctions.generate_tm_csrw(storedMsg, npc, isCompressed, t.insn, 0.U, 0.U)(params)
          isMsgStored := true
          isMsgReady := false
        }.elsewhen(csr_insn_ret) {
          if (params.debug) printf("[TV] [Tile] C0: %d : RET priv = %d | mstatus = 0x%x\n", time, t.priv, csr_mstatus)
          TVFunctions.generate_tm_ret(io.traceMsg.bits, npc, isCompressed, t.insn, t.priv, csr_mstatus)(params)
          isMsgReady := true
        }.elsewhen(isLoad) {
          if (params.debug) printf("[TV] [Tile] Load Instruction\n")
        }.elsewhen(isStore) {
          if (params.debug) printf("[TV] [Tile] Store Instruction: 0x%x @ 0x%x\n", stored_data, stored_addr)
          TVFunctions.generate_tm_store(io.traceMsg.bits, npc, isCompressed, t.insn, stored_data, stored_addr)(params)
          isMsgReady := true
        }.otherwise {
          TVFunctions.generate_tm_other(io.traceMsg.bits, npc, isCompressed, t.insn)(params)
          isMsgReady := true
        }
      }
  }.elsewhen(exception && !trapToDebug) {
    if (params.debug) printf("[TV] [Tile] Exception\n")
    // Use mem_npc, which is an earlier PC. This should be the PC that will be executed after the exception
    TVFunctions.generate_tm_trap(io.traceMsg.bits, mem_npc, isCompressed, t.insn, epriv, csr_mstatus, cause, epc, tval)
    isMsgReady := true
  }.elsewhen(t.interrupt) {
    if (params.debug) printf("[TV] [Tile] Interrupt\n")
//    TVFunctions.generate_tm_intr(io.traceMsg.bits, npc, epriv, csr_mstatus, cause, epc, tval)
    isMsgReady := false
  }.otherwise {
    isMsgReady := false
  }

  when(isMsgStored && rf_wen && rf_waddr === storedMsg.rd && !storedMsgIsCSR) {
    isMsgStored := 0
    io.traceMsg.bits := storedMsg
    io.traceMsg.bits.word1 := rf_wdata
    assert(branch === false)
    if (params.debug) printf("[TV] [Tile] C0: %d : Stored 0x%x into reg %d from 0x%x\n", time, rf_wdata, storedMsg.rd, stored_addr)
    storedMsgReady := true
  }.elsewhen(isMsgStored && storedMsgIsCSR) {
    isMsgStored := 0
    io.traceMsg.bits := storedMsg
    io.traceMsg.bits.word3 := csr_trace_waddr
    io.traceMsg.bits.word4 := csr_trace_wdata
    if (params.debug) printf("[TV] [Tile] C0: %d : Updated 0x%x in CSR 0x%x\n", time, csr_trace_wdata, csr_trace_waddr)
    storedMsgReady := true
  }.elsewhen(isMsgStored) {
    storedMsgReady := false
  }.otherwise {
    storedMsgReady := false
  }

  when (ll_wen && rf_waddr =/= UInt(0)) {
    if (params.debug) printf ("[TV] [Tile] C0: %d : x%d p%d 0x%x\n", time, rf_waddr, rf_waddr, rf_wdata)
    //    printf ("rf_wen = %d | ll_wen = %d | rf_waddr = %d | rd = %d | isMsgStored = %d | storedMsg.rd = %d\n", rf_wen, ll_wen, rf_waddr, rd, isMsgStored, storedMsg.rd)
    isStoreReady := false
    assert(branch === false)
    when (isLoad) { if (params.debug) printf("[TV] [Tile] Load Instruction Type 2\n") }
    when (isStore) {
      if (params.debug) printf("[TV] [Tile] Store Instruction Type 2: 0x%x @ 0x%x\n", stored_data, stored_addr)
      TVFunctions.generate_tm_store(io.traceMsg.bits, npc, isCompressed, t.insn, stored_data, stored_addr)(params)
      isStoreReady := true
    }
  }.otherwise {
    isStoreReady := false
  }

}