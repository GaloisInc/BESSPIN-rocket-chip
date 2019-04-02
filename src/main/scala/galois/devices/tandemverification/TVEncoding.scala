
// Galois, Inc
//
// Based on reference design and specifications from Bluespec, Inc.
//
// Maintainer: Dylan Hand <dhand@galois.com>

package galois.devices.tandemverification

import Chisel._
import freechips.rocketchip.rocket.PRV

// Contains useful enum objects to make code more readable and reusable
// Currently implements Trace Protocol Version 2018-11-20 / b1d1aa953e29331f7b367493fe4a00526f524079

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
  def trace_csr_write_alt = UInt(16, SZ)
}

object TraceInstrSize {
  def SZ = 1
  def isize_16bit = UInt(0, SZ)
  def isize_32bit = UInt(1, SZ)
}

object TraceEnc {
  def te_op_begin_group               = UInt(1, 8)
  def te_op_end_group                 = UInt(2, 8)
  def te_op_incr_pc                   = UInt(3, 8)
  def te_op_full_reg                  = UInt(4, 8)
  def te_op_incr_reg                  = UInt(5, 8)
  def te_op_incr_reg_OR               = UInt(6, 8)
  def te_op_addl_state                = UInt(7, 8)
  def te_op_mem_req                   = UInt(8, 8)
  def te_op_mem_rsp                   = UInt(9, 8)
  def te_op_hart_reset                = UInt(10, 8)
  def te_op_state_init                = UInt(11, 8)
  def te_op_16b_instr                 = UInt(16, 8)
  def te_op_32b_instr                 = UInt(17, 8)
  def te_mem_req_size_8               = UInt(0, 2)
  def te_mem_req_size_16              = UInt(1, 2)
  def te_mem_req_size_32              = UInt(2, 2)
  def te_mem_req_size_64              = UInt(3, 2)
  def te_mem_req_op_Load              = UInt(0, 8)
  def te_mem_req_op_Store             = UInt(1, 8)
  def te_mem_req_op_LR                = UInt(2, 8)
  def te_mem_req_op_SC                = UInt(3, 8)
  def te_mem_req_op_AMO_swap          = UInt(4, 8)
  def te_mem_req_op_AMO_add           = UInt(5, 8)
  def te_mem_req_op_AMO_xor           = UInt(6, 8)
  def te_mem_req_op_AMO_and           = UInt(7, 8)
  def te_mem_req_op_AMO_or            = UInt(8, 8)
  def te_mem_req_op_AMO_min           = UInt(9, 8)
  def te_mem_req_op_AMO_max           = UInt(10, 8)
  def te_mem_req_op_AMO_minu          = UInt(11, 8)
  def te_mem_req_op_AMO_maxu          = UInt(12, 8)
  def te_mem_req_op_ifetch            = UInt(13, 8)
  def te_mem_result_success           = UInt(0, 8)
  def te_mem_result_failure           = UInt(1, 8)
  def te_op_addl_state_priv           = UInt(1, 8)
  def te_op_addl_state_paddr          = UInt(2, 8)
  def te_op_addl_state_eaddr          = UInt(3, 8)
  def te_op_addl_state_data8          = UInt(4, 8)
  def te_op_addl_state_data16         = UInt(5, 8)
  def te_op_addl_state_data32         = UInt(6, 8)
  def te_op_addl_state_data64         = UInt(7, 8)
  def te_op_addl_state_mtime          = UInt(8, 8)
  def te_op_addl_state_pc_paddr       = UInt(9, 8)
  def te_op_addl_state_pc             = UInt(10, 8)
  def csr_addr_mstatus                = UInt(0x300, 12)
  def csr_addr_sstatus                = UInt(0x100, 12)
  def csr_addr_ustatus                = UInt(0x000, 12)
  def csr_addr_mcause                 = UInt(0x342, 12)
  def csr_addr_scause                 = UInt(0x142, 12)
  def csr_addr_ucause                 = UInt(0x042, 12)
  def csr_addr_mepc                   = UInt(0x341, 12)
  def csr_addr_sepc                   = UInt(0x141, 12)
  def csr_addr_uepc                   = UInt(0x041, 12)
  def csr_addr_mtval                  = UInt(0x343, 12)
  def csr_addr_stval                  = UInt(0x143, 12)
  def csr_addr_utval                  = UInt(0x043, 12)
  def csr_addr_dpc                    = UInt(0x7b1, 12)

  def setCSRAddrs(addrs: TraceCSRAddrs, priv: UInt)= {
    switch (priv) {
      is (UInt(PRV.U)) {
        addrs.status := TraceEnc.csr_addr_ustatus
        addrs.cause  := TraceEnc.csr_addr_ucause
        addrs.epc    := TraceEnc.csr_addr_uepc
        addrs.tval   := TraceEnc.csr_addr_utval
      }
      is (UInt(PRV.S)) {
        addrs.status := TraceEnc.csr_addr_sstatus
        addrs.cause  := TraceEnc.csr_addr_scause
        addrs.epc    := TraceEnc.csr_addr_sepc
        addrs.tval   := TraceEnc.csr_addr_stval
      }
      is (UInt(PRV.M)) {
        addrs.status := TraceEnc.csr_addr_mstatus
        addrs.cause  := TraceEnc.csr_addr_mcause
        addrs.epc    := TraceEnc.csr_addr_mepc
        addrs.tval   := TraceEnc.csr_addr_mtval
      }
    }
  }
}

class TraceCSRAddrs extends Bundle {
  val status = UInt(width =12)
  val cause = UInt(width = 12)
  val epc = UInt(width = 12)
  val tval = UInt(width = 12)
}
