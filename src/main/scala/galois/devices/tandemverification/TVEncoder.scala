
// Galois, Inc
//
// Based on reference design and specifications from Bluespec, Inc.
//
// Maintainer: Dylan Hand <dhand@galois.com>

package galois.devices.tandemverification

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.XLen
// util package is required to make toBool work properly. Do not remove!
import freechips.rocketchip.util._

// IO for the TV Encoder. Consumes a TraceMessage from a tap, produces a TraceVector which conforms to the Bluespec
// trace protocol for consumption by (eventually) the PCIe bridge device
class TVEncoderIO()(implicit p: Parameters) extends Bundle {
  val traceData = Decoupled(new TraceMessage).flip
  val encodedOutput = Decoupled(new TraceVector)
}

// The TV Encoder consumes a TraceMessage from one or more taps (currently one) and produces a TraceVector
// which conforms to the Bluespec trace protocol for consumption by (eventually) the PCIe bridge device
// Currently implements Version 2018-11-20 / b1d1aa953e29331f7b367493fe4a00526f524079
class TVEncoder(params: TandemVerificationParams)(implicit p: Parameters) extends Module() {
  val io = new TVEncoderIO()

  val RV64 = p(XLen) == 64

  // Messages come in over a decoupled interface (data, req, ack). This register will store the message while processing
  val storedMsg = Reg(new TraceMessage)
  val storedMsgValid = RegInit(Bool(false))

  // Need to stall the core if our queues fill up
  val tv_stall = RegInit(Bool(false))

  // Use two queues - one from the tap module and one to the consumer module
  // The consumer module requires a larger queue as the CPU does not stall instantly in the current configuration
  val inQueue = Module(new Queue(new TraceMessage, entries = 2, pipe = false, flow = false))
  val outQueue = Module(new Queue(new TraceVector, entries = 8, pipe = false, flow = false))
  inQueue.io.enq <> io.traceData
  io.encodedOutput <> outQueue.io.deq

  // The following helper functions are used to translate the internal register addresses to those used by the trace
  // protocol (similar to the RISCV Debug spec)
  def encodeCSRReg(addr: UInt) : UInt = {
    val encodedCSR = Wire(UInt(16.W))
    encodedCSR := Cat(0.U(11), addr)
    encodedCSR
  }

  def encodeGPRReg(addr: UInt) : UInt = {
    val encodedGPR = Wire(UInt(16.W))
    encodedGPR := 4096.U(16.W) + addr
    encodedGPR
  } // 4096.U(16) + addr

  def encodeFPRReg(addr: UInt) : UInt = {
    val encodedFPR = Wire(UInt(16.W))
    encodedFPR := 4128.U(16.W) + addr
    encodedFPR
  } // 4128.U(16) + addr

  // These functions pack common datatypes into the appropriate format
  def encodeReg(addr: UInt, data: UInt) : TraceVector = {
    val returnVec = Wire(new TraceVector)
    returnVec.vec(0) := TraceEnc.te_op_full_reg
    returnVec.vec(1) := addr(7, 0)
    returnVec.vec(2) := addr(15, 8)
    if (RV64) {
      for (i <- 0 to 7) { returnVec.vec(i+3) := data(8*(i+1)-1, 8*i) }
    } else {
      for (i <- 0 to 3) { returnVec.vec(i+3) := data(8*(i+1)-1, 8*i) }
    }
    returnVec.count := (if (RV64) 11.U else 7.U)
    returnVec
  }

  // TODO: add RV64 support
  def encodeEaddr(addr: UInt) : TraceVector = {
    val returnVec = Wire(new TraceVector)
    returnVec.vec(0) := TraceEnc.te_op_addl_state
    returnVec.vec(1) := TraceEnc.te_op_addl_state_eaddr
    if (RV64) {
      for (i <- 0 to 7) { returnVec.vec(i+2) := addr(8*(i+1)-1, 8*i) }
    } else {
      for (i <- 0 to 3) { returnVec.vec(i+2) := addr(8*(i+1)-1, 8*i) }
    }
    returnVec.count := (if (RV64) 10.U else 6.U)
    returnVec
  }

  def encodeInstr(isize: UInt, instr: UInt) : TraceVector = {
    val returnVec = Wire(new TraceVector)
    returnVec.count := Mux(isize === TraceInstrSize.isize_16bit, 3.U, 5.U)
    when (isize === TraceInstrSize.isize_16bit) {
      if (params.debug) printf("[TVE] Encoding a compressed instruction!!\n")
    }
    returnVec.vec(0) := Mux(isize === TraceInstrSize.isize_16bit, TraceEnc.te_op_16b_instr, TraceEnc.te_op_32b_instr)
    for (i <- 0 to 3) {
      returnVec.vec(i+1) := instr(8*(i+1)-1, 8*i)
    }
    returnVec
  }

  // TODO: add RV64 support
  def encodeMlen(data: UInt) : TraceVector = {
    val returnVec = Wire(new TraceVector)
    if (RV64) {
      for (i <- 0 to 7) { returnVec.vec(i) := data(8*(i+1)-1, 8*i) }
    } else {
      for (i <- 0 to 3) { returnVec.vec(i) := data(8*(i+1)-1, 8*i) }
    }
    returnVec.count := (if (RV64) 8.U else 4.U)
    returnVec
  }

  // TODO: add RV64 support
  def encodeMdata(msize: UInt, data: UInt) : TraceVector = {
    val returnVec = Wire(new TraceVector)
    if (RV64) {
      for (i <- 0 to 7) { returnVec.vec(i) := data(8*(i+1)-1, 8*i) }
    } else {
      for (i <- 0 to 3) { returnVec.vec(i) := data(8*(i+1)-1, 8*i) }
    }
    switch (msize) {
      is (TraceEnc.te_mem_req_size_8) {
        returnVec.count := 1.U
      }
      is (TraceEnc.te_mem_req_size_16) {
        returnVec.count := 2.U
      }
      is (TraceEnc.te_mem_req_size_32) {
        returnVec.count := 4.U
      }
      is (TraceEnc.te_mem_req_size_64) {
        returnVec.count := 8.U
      }
    }
    returnVec
  }

  def encodePriv(priv: UInt) : TraceVector = {
    val returnVec = Wire(new TraceVector)
    returnVec.vec(0) := TraceEnc.te_op_addl_state
    returnVec.vec(1) := TraceEnc.te_op_addl_state_priv
    returnVec.vec(2) := priv
    returnVec.count  := 3.U
    returnVec
  }

  def encodePC(pc: UInt) : TraceVector = {
    val returnVec = Wire(new TraceVector)
    returnVec.vec(0) := TraceEnc.te_op_addl_state
    returnVec.vec(1) := TraceEnc.te_op_addl_state_pc
    if (RV64) {
      for (i <- 0 to 7) { returnVec.vec(i+2) := pc(8*(i+1)-1, 8*i) }
    } else {
      for (i <- 0 to 3) { returnVec.vec(i+2) := pc(8*(i+1)-1, 8*i) }
    }
    returnVec.count := (if (RV64) 10.U else 6.U)
    returnVec
  }

  def encodeStore(msize: UInt, data: UInt) : TraceVector = {
    val returnVec = Wire(new TraceVector)
    returnVec.vec(0) := TraceEnc.te_op_addl_state
    returnVec.vec(1) := 4.U + msize // Shift msize up by 4 to match proper encoding
    if (RV64) {
      for (i <- 0 to 7) { returnVec.vec(i+2) := data(8*(i+1)-1, 8*i) }
    } else {
      for (i <- 0 to 3) { returnVec.vec(i+2) := data(8*(i+1)-1, 8*i) }
    }
    switch (msize) {
      is (TraceEnc.te_mem_req_size_8) {
        returnVec.count := 3.U
      }
      is (TraceEnc.te_mem_req_size_16) {
        returnVec.count := 4.U
      }
      is (TraceEnc.te_mem_req_size_32) {
        returnVec.count := 6.U
      }
      is (TraceEnc.te_mem_req_size_64) {
        returnVec.count := 10.U
      }
    }
    returnVec
  }

  // Helper function that takes a UInt and creates a tracevector-style representation of it
  // The tracevector is 72 bytes
  def uintToVec(in: UInt) : Vec[UInt] = {
    val out = Wire(Vec(72, UInt(8.W)))
    for (i <- 0 to 71) {
      out(i) := in.toBools.slice(i*8, 8*i+8).asUInt()
    }
    out
  }

  // When instruction is 16-bit, it is a bit more complicated to extract the memory size. Need to decode which
  // instruction is being committed.
  def extractMemSize(in: UInt) : UInt = {
    Mux(in(1,0).andR, in(13, 12).asUInt(),
      // If instruction is compressed, check bit 13 to determine if it is a C.SW or C.SD/C.SQ (quad not supported)
      Mux(in(13), TraceEnc.te_mem_req_size_64, TraceEnc.te_mem_req_size_32))
  }

  // This function is the main magic behind how the trace encoder works. It is a hardware implementation of taking an
  // arbitrary number of arbitrarily long byte vectors and combining them into a single packed array.
  // It takes "fields", a vector of TraceVectors (which is a tuple of a byte array and how many bytes are valid), the number of
  // fields that are valid, and produces a single output trace vector that contains the shifted data of every field.
  // The length of the output tmpVector is the sum of all valid fields.
  //
  // In simplistic pseudo-code:
  // j = 0
  // for field in fields:
  //   for (count,data) in field:
  //     final_vec[j+count:j] = data
  //     j = j + count
  //
  // Note that convertedFields is just a temporary data structure that does not get used outside of this function
  def fieldsToTraceVector(convertedFields: Vec[UInt], fields: Vec[TraceVector], num: Int) :  TraceVector = {
    val tmpVector = Wire(new TraceVector)
    for (i <- 0 to (num-1)) {
      if (i == 0)
        convertedFields(i) := fields(i).asShiftedUInt() | fields(i+1).asShiftedUInt(8)
      else
        convertedFields(i) := convertedFields(i-1) | fields(i+1).asShiftedUInt((fields.slice(0,i+1).map(_.count).reduce(_+_) << 3).asUInt())
    }
    tmpVector.vec := uintToVec(convertedFields(num-1))
    // Note that count here is always +1. This is for the last field that will be added later (end group msg)
    tmpVector.count := 1+fields.slice(0, num+1).map(_.count).reduce(_+_)
    tmpVector
  }

  // Need to convert traceMsg into a byte vector that can be streamed out to PCIe block
  //
  // Need a vector of 72 bytes + counter to track how many bytes are in used/valid. Not all messages are 72 bytes long
  //
  // Should register the tracemessage on one cycle, convert to a tracevector on 2nd cycle, and then make it available on third cycle

  val encVec = Wire(new TraceVector)

  // Get the traceMsg and store it
  when (inQueue.io.deq.valid && outQueue.io.enq.ready) {
    if (params.debug) printf("MSGTRACKER: Received Message\n")
    inQueue.io.deq.ready := true
    storedMsgValid := true
    storedMsg := inQueue.io.deq.bits
  }.otherwise {
    inQueue.io.deq.ready := false
    storedMsgValid := false
  }

  // Stall when there is something in the output queue *or* we are about to put something in the queue
  tv_stall := (outQueue.io.count >= 6.U) // | outQueue.io.enq.valid

  // Debug
  when (tv_stall) {
    if (params.debug) printf("[TV] CPU Stalled. inQueue = %d | outQueue = %d\n", inQueue.io.count, outQueue.io.count)
  }

  var endPosVec = Wire(Vec(5, UInt(7.W)))
  val fields = Wire(Vec(8, new TraceVector))
  val convertedFields = Wire(Vec(8, UInt(576.W)))

  val csr_addr = Wire(new TraceCSRAddrs)
  val csr_addr_status = Wire(UInt(width = 32))
  val csr_addr_cause  = Wire(UInt(width = 32))
  val csr_addr_epc    = Wire(UInt(width = 32))
  val csr_addr_tval   = Wire(UInt(width = 32))

  // Convert the storedMsg
  when (storedMsgValid && storedMsg.pc >= 0x8000000.U) {
    outQueue.io.enq.valid := false
    fields(0).vec(0) := TraceEnc.te_op_begin_group
    fields(0).count := 1.U
    switch (storedMsg.op) {
      is (TraceOP.trace_reset) {
        if(params.debug) printf("[TVE] Encoding reset\n")
        encVec.vec := fields(0).vec
        encVec.vec(1) := TraceEnc.te_op_hart_reset
        encVec.vec(2) := TraceEnc.te_op_end_group
        encVec.count  := 3
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_i_rd) {
        if(params.debug) printf("[TVE] Encoding IRD | pc = 0x%x | rd = 0x%x\n", storedMsg.pc, encodeGPRReg(storedMsg.rd).asUInt())
        fields(1) := encodePC(storedMsg.pc)
        fields(2) := encodeInstr(storedMsg.instr_size, storedMsg.instr)
        fields(3) := encodeReg(encodeGPRReg(storedMsg.rd), storedMsg.word1)
        encVec := fieldsToTraceVector(convertedFields, fields, 3)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_i_load) {
        if(params.debug) printf("[TVE] Encoding ILD | pc = 0x%x\n", storedMsg.pc)
        fields(1) := encodePC(storedMsg.pc)
        fields(2) := encodeInstr(storedMsg.instr_size, storedMsg.instr)
        fields(3) := encodeReg(encodeGPRReg(storedMsg.rd), storedMsg.word1)
        fields(4) := encodeEaddr(storedMsg.word3)
        encVec := fieldsToTraceVector(convertedFields, fields, 4)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_store) {
        if(params.debug) printf("[TVE] Encoding ISR | pc = 0x%x\n", storedMsg.pc)
        fields(1) := encodePC(storedMsg.pc)
        fields(2) := encodeInstr(storedMsg.instr_size, storedMsg.instr)
        fields(3) := encodeStore(extractMemSize(storedMsg.instr), storedMsg.word2)
        fields(4) := encodeEaddr(storedMsg.word3)
        encVec := fieldsToTraceVector(convertedFields, fields, 4)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_amo) {
        if(params.debug) printf("[TVE] Encoding AMO | pc = 0x%x\n", storedMsg.pc)
        fields(1) := encodePC(storedMsg.pc)
        fields(2) := encodeInstr(storedMsg.instr_size, storedMsg.instr)
        fields(3) := encodeReg(encodeGPRReg(storedMsg.rd), storedMsg.word1)
        fields(4) := encodeStore(extractMemSize(storedMsg.instr), storedMsg.word2)
        fields(5) := encodeEaddr(storedMsg.word3)
        encVec := fieldsToTraceVector(convertedFields, fields, 5)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_csrrx) {
        if(params.debug) printf("[TVE] Encoding CSR | pc = 0x%x\n", storedMsg.pc)
        fields(1) := encodePC(storedMsg.pc)
        fields(2) := encodeInstr(storedMsg.instr_size, storedMsg.instr)
        fields(3) := encodeReg(encodeGPRReg(storedMsg.rd), storedMsg.word1)
        when (storedMsg.word2 === 0.U) {
          fields(4).count := 0
        } .otherwise {
          fields(4) := encodeReg(encodeCSRReg(storedMsg.word3), storedMsg.word4)
        }
        encVec := fieldsToTraceVector(convertedFields, fields, 4)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_ret) {
        if(params.debug) printf("[TVE] Encoding RET | pc = 0x%x\n", storedMsg.pc)
        fields(1) := encodePC(storedMsg.pc)
        fields(2) := encodeInstr(storedMsg.instr_size, storedMsg.instr)
        fields(3) := encodePriv(storedMsg.rd)
        fields(4) := encodeReg(encodeCSRReg(TraceEnc.csr_addr_mstatus), storedMsg.word1)
        encVec := fieldsToTraceVector(convertedFields, fields, 4)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_other) {
        if(params.debug) printf("[TVE] Encoding OTR | pc = 0x%x\n", storedMsg.pc)
        fields(1) := encodePC(storedMsg.pc)
        fields(2) := encodeInstr(storedMsg.instr_size, storedMsg.instr)
        encVec := fieldsToTraceVector(convertedFields, fields, 2)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_csr_write) {
        if(params.debug) printf("[TVE] Encoding CSW | pc = 0x%x\n", storedMsg.pc)
        fields(1).vec(0) := TraceEnc.te_op_state_init
        fields(1).count  := 1.U
        fields(2) := encodeReg(encodeCSRReg(storedMsg.word3), storedMsg.word4)
        encVec := fieldsToTraceVector(convertedFields, fields, 2)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_trap) {
        if(params.debug) printf("[TVE] Encoding TRP | pc = 0x%x\n", storedMsg.pc)
        // Some setup work first
        TraceEnc.setCSRAddrs(csr_addr, storedMsg.rd)
        // Always send the instruction for now. Could optimize later
        fields(1) := encodePC(storedMsg.pc)
        fields(2) := encodeInstr(storedMsg.instr_size, storedMsg.instr)
        fields(3) := encodePriv(storedMsg.rd)
        fields(4) := encodeReg(encodeCSRReg(csr_addr.status), storedMsg.word1)
        fields(5) := encodeReg(encodeCSRReg(csr_addr.cause),  storedMsg.word2)
        fields(6) := encodeReg(encodeCSRReg(csr_addr.epc),    storedMsg.word3)
        fields(7) := encodeReg(encodeCSRReg(csr_addr.tval),   storedMsg.word4)
        encVec := fieldsToTraceVector(convertedFields, fields, 7)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_intr) {
        if(params.debug) printf("[TVE] Encoding ITR | pc = 0x%x\n", storedMsg.pc)
        // Some setup work first
        TraceEnc.setCSRAddrs(csr_addr, storedMsg.rd)
        fields(1) := encodePC(storedMsg.pc)
        fields(2) := encodePriv(storedMsg.rd)
        fields(3) := encodeReg(encodeCSRReg(csr_addr.status), storedMsg.word1)
        fields(4) := encodeReg(encodeCSRReg(csr_addr.cause),  storedMsg.word2)
        fields(5) := encodeReg(encodeCSRReg(csr_addr.epc),    storedMsg.word3)
        fields(6) := encodeReg(encodeCSRReg(csr_addr.tval),   storedMsg.word4)
        encVec := fieldsToTraceVector(convertedFields, fields, 6)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_gpr_write) {
        if(params.debug) printf("[TVE] Encoding GPW | loc = 0x%x\n", storedMsg.rd)
        fields(1).vec(0) := TraceEnc.te_op_state_init
        fields(1).count  := 1.U
        fields(2) := encodeReg(encodeGPRReg(storedMsg.rd), storedMsg.word1)
        encVec := fieldsToTraceVector(convertedFields, fields, 2)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
      is (TraceOP.trace_mem_write) {
        if(params.debug) printf("[TVE] Encoding MEM\n")
        fields(1).vec(0) := TraceEnc.te_op_state_init
        fields(1).count  := 1.U
        fields(2).vec(1) := TraceEnc.te_op_mem_req
        fields(2).count  := 1.U
        fields(3)        := encodeMlen(storedMsg.word3)
        //in.toBools.slice(i*8, 8*i+8).asUInt()
        fields(4).vec(0) := Cat(0.U(2.W), storedMsg.word1.toBools.slice(0,1).asUInt(), TraceEnc.te_mem_req_op_Store)
        fields(4).count  := 1.U
        fields(5)        := encodeMdata(storedMsg.word1, storedMsg.word2)
        encVec := fieldsToTraceVector(convertedFields, fields, 5)
        encVec.vec(encVec.count - 1) := TraceEnc.te_op_end_group
        outQueue.io.enq.valid := true
      }
    }
    if (params.debug) {
     when(outQueue.io.enq.valid) {
        printf("%d %d encVec = 0x%x\n", outQueue.io.enq.ready, outQueue.io.count, encVec.vec.asUInt())
      }.otherwise {
        printf("MSGTRACKER: Unsupported Message!\n")
      }
    }
    outQueue.io.enq.bits := encVec

  } .otherwise {
    outQueue.io.enq.valid := false
  }
}