// check hart id
// if hart id == 0:
//   memcopy operands
//   signal to other harts?
//   jump to 80000000
// else
//   jump to 80000000

// boot all cores (only hart 0) and jump to main program execution
.section .text.start, "ax", @progbits
.globl _start
_start:
  nop               // nop for 8 cycles
  nop               // to wait for irq deassert
  nop
  nop
  nop
  nop
  nop
  nop
  j j_target
j_target:
  li a1, 0x2000000  // base address of clint
  csrr a0, mhartid
  bnez a0, boot_core

// copy args from ROM to heap
  li a2, 0x7fff0000 // args
  lw a3, 52(a2)     // stack ptr to heap operand a address
  lw a3, 0(a3)      // heap operand a address
  // lw s0, 64(a2)  // operand a size
  li s0, 1024         // hardcode 64 elements
  li a2, 0x20000    // rom operand a address
  li t0, 0

copy_op_a:
  bge t0, s0, end_op_a
  lw t1, 0(a2)
  sw t1, 0(a3)
  addi t0, t0, 1    // increment index
  addi a2, a2, 4    // increment ROM memory address
  addi a3, a3, 4    // increment heap memory address
  j copy_op_a

end_op_a:
  li a2, 0x7fff0000 // args
  lw a3, 56(a2)     // stack ptr to heap operand b address
  lw a3, 0(a3)      // heap operand b address
  // lw s0, 68(a2)  // operand b size
  li s0, 1024         // hardcode 64 elements
  li a2, 0x28000    // rom operand b address
  li t0, 0

copy_op_b:
  bge t0, s0, end_op_b
  lw t1, 0(a2)
  sw t1, 0(a3)
  addi t0, t0, 1    // increment index
  addi a2, a2, 4    // increment ROM memory address
  addi a3, a3, 4    // increment heap memory address
  j copy_op_b

end_op_b:
  addi a2, a1, 4    // irq addr for hart 1
  li a3, 1
interrupt_loop:     // trigger MSIP interrupts for other harts so that they also boot
  sw a3, 0(a2)
  addi a2, a2, 4
  lw a3, -4(a2)
  bnez a3, interrupt_loop
  j boot_core_hart0

// this boot ROM doesn't know about any boot devices, so it just spins,
// waiting for the serial interface to load the program and interrupt it
.section .text.hang100, "ax", @progbits
.globl _hang100
_hang100: // reset vector
  la a0, _start          // on MSIP interrupt, go to _start
  csrw mtvec, a0

wfi_loop: // wait for MSIP interrupt to start program execution
  nop                // vortex has no support for wfi
  j wfi_loop

// other harts must wait until MSIP of hart 0 is cleared to continue
boot_core:
  lw t0, 0(a1)       // load hart 0 MSIP
  bnez t0, boot_core // block until hart 0 clears own MSIP
  sll a0, a0, 2      // offset for hart MSIP
  add a1, a0, a1
boot_core_hart0:     // begin executing code at DRAM_BASE
  sw zero, 0(a1)     // clear the interrupt

  li a0, 0           // clear registers
  li a1, 0           // clear registers
  li a2, 0           // clear registers
  li a3, 0           // clear registers
  li t1, 0           // clear registers
  li s0, 0           // clear registers

  li t0, 0x80000000  // load DRAM start
  jr t0

.align 3
_dtb:
