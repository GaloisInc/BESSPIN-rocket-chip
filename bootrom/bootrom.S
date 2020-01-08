#define DRAM_BASE 0x80000000
#define UART_REG 0x6000101C

.section .text.start, "ax", @progbits
.globl _start
_start:
  # csrwi 0x7c1, 0 // disable chicken bits
  # csrr a0, mhartid
  # la a1, _dtb
  # csrwi mie, 0
  li t0, DRAM_BASE
  li t2, 0xdead
  sw t2, 0(t0)
  lw t3, 0(t0)
  li t0, UART_REG
  sw t2, 0(t0)
1:
  wfi
  j 1b

.section .text.hang, "ax", @progbits
.globl _hang
_hang:
  csrwi 0x7c1, 0 // disable chicken bits
  csrr a0, mhartid
  la a1, _dtb
  csrwi mie, 0
  # li t0, DRAM_BASE
  # li t2, 0xbeef
  # sw t2, 0(t0)
  # lw t3, 0(t0)
  # li t0, UART_REG
  # sw t2, 0(t0)
  # lw t3, 0(t0)
1:
  wfi
  j 1b

.section .rodata.dtb, "a", @progbits
.globl _dtb
.align 5, 0
_dtb:
.ascii "DTB goes here"
