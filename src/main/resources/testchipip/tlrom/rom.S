#define DRAM_BASE 0x80000000

// Dummy empty alternate boot rom.
// Immediately jumps to DRAM_BASE
.section .text.start, "ax", @progbits
.globl _start
_start:
	li a0, DRAM_BASE
	jalr a0
