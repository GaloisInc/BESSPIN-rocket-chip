
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
  val mulDiv_valid  = Wire(Bool())
  val wb_wen        = Wire(Bool())
  val rf_wen        = Wire(Bool())
  val has_data      = wb_wen && !wb_set_sboard
  val time          = Wire(UInt(width = 32))
  val dmem_addr     = Wire(UInt(width = if (p(XLen) == 64) 64 else 32))
  val stored_addr   = Reg(next = Reg(next = dmem_addr))
  val dmem_data     = Wire(UInt(width = p(XLen)))
  val stored_data_old   = Reg(next = Reg(next = dmem_data))
  val stored_data   = dmem_data
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

  // Want valid to be registered but 'done'/consumed ack to be combinational, so a custom interface is used
  val capturedMsg = Reg(new TraceMessage)
  val capturedMsg_valid = RegInit(false.B)
  val capturedMsg_done  = Wire(Bool())
  val inReset     = Reg(init = Bool(true))

  val msgIsCSR = Wire(Bool())
  val capturedMsgIsCSR = Wire(Bool())
  val capturedMsgNeedsData = RegInit(false.B)

  //  when (mip_update) {
  //    if (params.debug) printf("[TV] [Tile] %d MIP has changed! mip = 0x%x\n", time, mipval)
  //    // TODO: Replace with correct MIP address
  //    TVFunctions.generate_tm_csrw(io.traceMsg.bits, UInt(36), mipval)(params)
  //  }

  val isNop = Wire(Bool())
  isNop := t.insn === 0x00000013.U

  capturedMsgIsCSR := capturedMsg.op === TraceOP.trace_csr_write

//  printf("[TV] [Tile] C0: %d | npc = 0x%x | mem_npc = 0x%x\n", time, npc, mem_npc)

  when(fpu_load_wb) {
    if (params.debug) printf("[TV] [Tile] [FP] [%d] Addr = 0x%x | Data = 0x%x!\n", time, fpu_tag, fpu_data)
  }

//  if (params.debug) printf("[TV] [Tile] [STORE] C0: %d | dmem_data = 0x%x | stored_data = 0x%x", time, dmem_data, stored_data_old)

  // Need to reconfigure how this works. It will be separated into two stages, capture and post-process
  // In most cases, the post-process stage will do nothing
  // For:
  // -- CSR instructions it will fill in the final value of the CSR data, which is available only 1 cycle after the
  // instruction is committed
  // -- Speculatively committed Loads/Stores it will fill in the final data value once it is available approx. 4 cycles
  // later.
  // The first stage must check if a valid instruction is waiting, and properly detect issues:
  // -- if waiting and isCSR, it is fine to write the next committed instruction (CSR data is always available next cycle)
  // -- if waiting and not isCSR and new instruction is not NOP, raise an error.

  when(inReset) {
    inReset := false
    TVFunctions.generate_tm_reset(capturedMsg)(params)
    capturedMsg_valid := true.B
  }.elsewhen(t.valid && !t.exception) {
    // Make sure we don't clear out a previously stored message if a nop is committed
    // This condition will no longer hold when releasing instruction early - a different warning is printed instead.
    when(capturedMsg_valid && !capturedMsgIsCSR && !capturedMsg_done) {
      when(isNop === false.B) {
        if (params.debug) printf("[TV] [Tile] [WARN] New instruction committed before previously stored instruction sent to TraceEncoder. Check instruction stream!\n")
      }
    }

    capturedMsg_valid := false
    capturedMsgNeedsData := false.B

    when(wfd) {
      if (params.debug) printf("[TV] [Tile] C0: %d : %d 0x%x (0x%x) f%d p%d 0xXXXXXXXXXXXXXXXX\n", time, t.priv, t.iaddr, t.insn, rd, rd + UInt(32))
      if (params.debug) printf("[TV] [Tile] Floating point instruction\n")
    }
      .elsewhen(wxd && rd =/= UInt(0) && has_data) {
        // assert(branch === false)

        if (params.debug) printf("[TV] [Tile] C0: %d : %d 0x%x (0x%x) x%d 0x%x addr = 0x%x\n", time, t.priv, t.iaddr, t.insn, rd, rf_wdata, stored_addr)

        when(isCSRRX) {
          if (params.debug) printf("[TV] [Tile] C0: %d : CSRRX Instruction: CSR Addr = 0x%x | CSR Data = 0x%x\n", time, csr_addr, csr_wdata)
          TVFunctions.generate_tm_csrrx(capturedMsg, npc, isCompressed, t.insn, rd, rf_wdata, Bool(true), csr_addr, csr_wdata)
        }.elsewhen(isAMOInsn) {
          TVFunctions.generate_tm_amo(capturedMsg, npc, isCompressed, t.insn, rd, rf_wdata, stored_data, stored_addr)
        }.otherwise {
          TVFunctions.generate_tm_i(capturedMsg, isLoad, npc, isCompressed, t.insn, rd, rf_wdata, stored_addr)(params)
        }
        capturedMsg_valid := true.B
        capturedMsgNeedsData := false.B
      }
      .elsewhen(wxd && rd =/= UInt(0) && !has_data) {
        when (!isLoad && wb_set_sboard) {
          TVFunctions.generate_tm_other(capturedMsg, npc, isCompressed, t.insn)(params)
          capturedMsgNeedsData := false.B
          if (params.debug) printf("[TV] [Tile] C0: %d : Received instruction that requires scoreboard. Tracing final register update not currently supported!\n", time)
        }.otherwise {
          TVFunctions.generate_tm_i(capturedMsg, isLoad, npc, isCompressed, t.insn, rd, rf_wdata, stored_addr)(params)
          // assert(branch === false)
          capturedMsgNeedsData := true.B
          if (params.debug) printf("[TV] [Tile] C0: %d : Stored Message for rd = %d\n", time, rd)
        }
        capturedMsg_valid := true.B
        if (params.debug) printf("[TV] [Tile] C0: %d : %d 0x%x (0x%x) x%d p%d 0xXXXXXXXXXXXXXXXX addr = 0x%x\n", time, t.priv, t.iaddr, t.insn, rd, rd, stored_addr)
      }
      .otherwise {
        if (params.debug) printf("[TV] [Tile] C0: %d : %d 0x%x (0x%x)\n", time, t.priv, t.iaddr, t.insn)
        when(branch) {
          if (params.debug) printf("[TV] [Tile] C0: %d : Branch npc = 0x%x\n", time, npc)
          TVFunctions.generate_tm_other(capturedMsg, npc, isCompressed, t.insn)(params)
        }.elsewhen(csr_wen) {
          if (params.debug) printf("[TV] [Tile] C0: %d : CSR Write Enabled\n", time)
          TVFunctions.generate_tm_csrw(capturedMsg, npc, isCompressed, t.insn, 0.U, 0.U)(params)
        }.elsewhen(csr_insn_ret) {
          if (params.debug) printf("[TV] [Tile] C0: %d : RET priv = %d | mstatus = 0x%x\n", time, t.priv, csr_mstatus)
          TVFunctions.generate_tm_ret(capturedMsg, npc, isCompressed, t.insn, t.priv, csr_mstatus)(params)
        }.elsewhen(isLoad) {
          if (params.debug) printf("[TV] [Tile] Unimplemented Load Instruction\n")
        }.elsewhen(isStore) {
          if (params.debug) printf("[TV] [Tile] Store Instruction: 0x%x @ 0x%x\n", stored_data, stored_addr)
          TVFunctions.generate_tm_store(capturedMsg, npc, isCompressed, t.insn, stored_data, stored_addr)(params)
        }.otherwise {
          TVFunctions.generate_tm_other(capturedMsg, npc, isCompressed, t.insn)(params)
        }
        capturedMsg_valid := true.B
        capturedMsgNeedsData := false.B
      }
  }.elsewhen(exception && !trapToDebug) {
    if (params.debug) printf("[TV] [Tile] Exception | pc = 0x%x\n", mem_npc)
    // Use mem_npc, which is an earlier PC. This should be the PC that will be executed after the exception
    TVFunctions.generate_tm_trap(capturedMsg, mem_npc, isCompressed, t.insn, epriv, csr_mstatus, cause, epc, tval)
    capturedMsg_valid := true.B
    capturedMsgNeedsData := false.B
  }.elsewhen(t.interrupt) {
    if (params.debug) printf("[TV] [Tile] Interrupt\n")
    //    TVFunctions.generate_tm_intr(io.traceMsg.bits, npc, epriv, csr_mstatus, cause, epc, tval)
    capturedMsg_valid := false.B
    capturedMsgNeedsData := false.B
  }.elsewhen (ll_wen  && !mulDiv_valid && rf_waddr =/= UInt(0)) {
    // Add condition on mulDiv_valid to make sure we're not trying to update a previously executed Mul/Div instruction
    // Currently, scoreboard operations are not supported beyond waiting for D$ updates. RoCC also should be checked
    // if that is used.
    if (params.debug) printf ("[TV] [Tile] C0: %d : x%d p%d 0x%x\n", time, rf_waddr, rf_waddr, rf_wdata)
    when (isLoad) { if (params.debug) printf("[TV] [Tile] Load Instruction Type 2\n") }
    when (isStore) {
      if (params.debug) printf("[TV] [Tile] Store Instruction Type 2: 0x%x @ 0x%x\n", stored_data, stored_addr)
      TVFunctions.generate_tm_store(capturedMsg, npc, isCompressed, t.insn, stored_data, stored_addr)(params)
      capturedMsg_valid := true.B
      capturedMsgNeedsData := false.B
    }.elsewhen (capturedMsg_valid && capturedMsg_done) {
      // Check if some other condition in captureMsg has been met that allowed it to finish. If so, we
      // should not keep the msg valid
      capturedMsg_valid := false.B
      capturedMsgNeedsData := false.B
    }
  }.elsewhen(capturedMsg_valid && capturedMsg_done) {
    capturedMsg_valid := false.B
    capturedMsgNeedsData := false.B
  }

  io.traceMsg.bits := capturedMsg

  when(capturedMsg_valid) {
    when(capturedMsgNeedsData && rf_wen && rf_waddr === capturedMsg.rd  && !mulDiv_valid && !capturedMsgIsCSR) {
      capturedMsg_done := true.B
      io.traceMsg.bits.word1 := rf_wdata
      // assert(branch === false)
      if (params.debug) printf("[TV] [Tile] C0: %d : Stored 0x%x into reg %d from 0x%x\n", time, rf_wdata, capturedMsg.rd, stored_addr)
      io.traceMsg.valid := true.B
    }.elsewhen(capturedMsgIsCSR) {
      capturedMsg_done := true.B
      io.traceMsg.bits.word3 := csr_trace_waddr
      io.traceMsg.bits.word4 := csr_trace_wdata
      if (params.debug) printf("[TV] [Tile] C0: %d : Updated 0x%x in CSR 0x%x\n", time, csr_trace_wdata, csr_trace_waddr)
      io.traceMsg.valid := true.B
    }.elsewhen(!capturedMsgNeedsData) {
      if (params.debug) printf("[TV] [Tile] C0: %d : No data needed. Passing on traceMsg (pc = 0x%x)\n", time, capturedMsg.pc)
      io.traceMsg.valid := true.B
      capturedMsg_done := true.B
    }.elsewhen(capturedMsgNeedsData && t.valid && !t.exception && !isNop) {
      // We are storing a message that needs data, but the CPU has already committed an unrelated instruction
      // This is a temporary work-around to push the instruction to the trace with incorrect data and/or effective address
      // while a more robust solution is developed
      if (params.debug) {
        printf("[TV] [Tile] [WARN] C0: %d : New instruction committed before data was available for previous instruction." +
        " Passing on traceMsg with possibly incorrect data (pc = 0x%x)\n", time, capturedMsg.pc)
      }
      io.traceMsg.valid := true.B
      capturedMsg_done := true.B
    }.otherwise {
      io.traceMsg.valid := false.B
      capturedMsg_done := false.B
    }
  }.otherwise {
    io.traceMsg.valid := false.B
    capturedMsg_done := false.B
  }

}
