
// Galois, Inc
//
// Based on reference design and specifications from Bluespec, Inc.
//
// Maintainer: Dylan Hand <dhand@galois.com>

package galois.devices.tandemverification

import Chisel._

// Functions to generate TraceMessages that the TV Encoder expects from raw data. These functions can be reused across
// different types of TV taps.

object TVFunctions {

  def generate_tm_csrw(msg: TraceMessage, csraddr: UInt, csrval: UInt)(implicit params: TandemVerificationParams) = {
    msg.op := TraceOP.trace_csr_write
    msg.word3 := csraddr
    msg.word4 := csrval
    if (params.debug) printf("MSGTRACKER: New Message\n")
  }

  def generate_tm_i_rd(msg: TraceMessage, pc: UInt, compressed: Bool, instruction: UInt, rd: UInt, rdvalue: UInt) = {
    msg.op := TraceOP.trace_i_rd
    msg.pc := pc
    msg.instr_size := Mux(compressed, TraceInstrSize.isize_16bit, TraceInstrSize.isize_32bit)
    msg.instr := instruction
    msg.rd := rd
    msg.word1 := rdvalue
  }

  def generate_tm_i_load(msg: TraceMessage, pc: UInt, compressed: Bool, instruction: UInt, rd: UInt, rdvalue: UInt,
                         eaddr: UInt)(implicit params: TandemVerificationParams) = {
    generate_tm_i_rd(msg, pc, compressed, instruction, rd, rdvalue)
    msg.op := TraceOP.trace_i_load
    msg.word3 := eaddr
  }

  def generate_tm_i(msg: TraceMessage, isLoad: Bool, pc: UInt, compressed: Bool, instruction: UInt, rd: UInt, rdvalue: UInt,
                    eaddr: UInt)(implicit params: TandemVerificationParams) = {
    when (isLoad) {
      generate_tm_i_load(msg, pc, compressed, instruction, rd, rdvalue, eaddr)
    }.otherwise {
      generate_tm_i_rd(msg, pc, compressed, instruction, rd, rdvalue)
    }
    if (params.debug) printf("MSGTRACKER: New Message\n")
  }

  def generate_tm_amo(msg: TraceMessage, pc: UInt, compressed: Bool, instruction: UInt, rd: UInt, rdvalue: UInt,
                      data: UInt, eaddr: UInt) = {
    msg.op := TraceOP.trace_amo
    msg.pc := pc
    msg.instr_size := Mux(compressed, TraceInstrSize.isize_16bit, TraceInstrSize.isize_32bit)
    msg.instr := instruction
    msg.rd := rd
    msg.word1 := rdvalue
    msg.word2 := data
    msg.word3 := eaddr
    //    printf("MSGTRACKER: New Message\n")
  }

  def generate_tm_csrrx(msg: TraceMessage, pc: UInt, compressed: Bool, instruction: UInt, rd: UInt, rdvalue: UInt,
                        csrvalid: Bool, csraddr: UInt, csrval: UInt) = {
    msg.op := TraceOP.trace_csrrx
    msg.pc := pc
    msg.instr_size := Mux(compressed, TraceInstrSize.isize_16bit, TraceInstrSize.isize_32bit)
    msg.instr := instruction
    msg.rd := rd
    msg.word1 := rdvalue
    msg.word2 := csrvalid
    msg.word3 := csraddr
    msg.word4 := csrval
    //   printf("MSGTRACKER: New Message\n")
  }

  def generate_tm_store(msg: TraceMessage, pc: UInt, compressed: Bool, instruction: UInt, data: UInt,
                        eaddr: UInt)(implicit params: TandemVerificationParams) = {
    msg.op := TraceOP.trace_store
    msg.pc := pc
    msg.instr_size := Mux(compressed, TraceInstrSize.isize_16bit, TraceInstrSize.isize_32bit)
    msg.instr := instruction
    msg.word2 := data
    msg.word3 := eaddr
    if (params.debug) printf("MSGTRACKER: New Message\n")
  }

  def generate_tm_other(msg: TraceMessage, pc: UInt, compressed: Bool,
                        instruction: UInt)(implicit params: TandemVerificationParams) = {
    msg.op := TraceOP.trace_other
    msg.pc := pc
    msg.instr_size := Mux(compressed, TraceInstrSize.isize_16bit, TraceInstrSize.isize_32bit)
    msg.instr := instruction
    if (params.debug) printf("MSGTRACKER: New Message\n")
  }

  def generate_tm_ret(msg: TraceMessage, pc: UInt, compressed: Bool, instruction: UInt, priv: UInt,
                      mstatus: UInt)(implicit params: TandemVerificationParams) = {
    msg.op := TraceOP.trace_ret
    msg.pc := pc
    msg.instr_size := Mux(compressed, TraceInstrSize.isize_16bit, TraceInstrSize.isize_32bit)
    msg.instr := instruction
    msg.rd := priv
    msg.word1 := mstatus
    if (params.debug) printf("MSGTRACKER: New Message\n")
  }

  // TODO: Implement call if necessary
  def generate_tm_gprw(msg: TraceMessage, rd: UInt, rdvalue: UInt) = {
    msg.op := TraceOP.trace_gpr_write
    msg.rd := rd
    msg.word1 := rdvalue
  }

  // TODO: Implement call if necessary
  def generate_tm_memw(msg: TraceMessage, memsize: UInt, mvalue: UInt, paddr: UInt) = {
    msg.op := TraceOP.trace_mem_write
    msg.word1 := memsize
    msg.word2 := mvalue
    msg.word3 := paddr
  }

  // TODO: Implement call if necessary
  def generate_tm_intr(msg: TraceMessage, pc: UInt, priv: UInt, mstatus: UInt, mcause: UInt,
                       mepc: UInt, mtval: UInt) = {
    msg.op := TraceOP.trace_intr
    msg.pc := pc
    msg.rd := priv
    msg.word1 := mstatus
    msg.word2 := mcause
    msg.word3 := mepc
    msg.word4 := mtval
  }

  // TODO: Check functionality
  def generate_tm_trap(msg: TraceMessage, pc: UInt, compressed: Bool, instruction: UInt, priv: UInt, mstatus: UInt,
                       mcause: UInt, mepc: UInt, mtval: UInt) = {

    msg.op := TraceOP.trace_trap
    msg.pc := pc
    msg.instr_size := Mux(compressed, TraceInstrSize.isize_16bit, TraceInstrSize.isize_32bit)
    msg.instr := instruction
    msg.rd := priv
    msg.word1 := mstatus
    msg.word2 := mcause
    msg.word3 := mepc
    msg.word4 := mtval
  }

  def generate_tm_reset(msg: TraceMessage)(implicit params: TandemVerificationParams) = {
    msg.op := TraceOP.trace_reset
    if (params.debug) printf("MSGTRACKER: New Message\n")
    if (params.debug) printf("Reset Trace\n")
  }
}