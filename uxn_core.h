#ifndef __UXN_CORE_H__
#define __UXN_CORE_H__
/*
Copyright (c) 2022 Devine Lu Linvega, Andrew Richards

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
*/
#pragma warning(disable:4244) /* Noisy VC6 warning. Can't disable with flag */
typedef unsigned char UxnU8;
typedef signed char UxnI8;
typedef unsigned short UxnU16;

typedef struct UxnStack {
	UxnU8 ptr, dat[255];
} UxnStack;

typedef struct UxnCore {
	UxnU8 *ram;
	UxnStack *wst, *rst;
	UxnU8 (*dei)(struct UxnCore *u, unsigned int address);
	void (*deo)(struct UxnCore *u, unsigned int address, unsigned int value);
	UxnU16 pc, fault_code;
} UxnCore;

#define UXN_FAULT_DONE 1
#define UXN_FAULT_STACK_UNDERFLOW 2
#define UXN_FAULT_STACK_OVERFLOW 3
#define UXN_FAULT_DIVIDE_BY_ZERO 4

/* Runs up to 'limit' number of Uxn instructions.
   Returns limit - (number of instructions executed).
   Execution starts at address 'u->pc', so set it (or leave it) as needed.
   `u->fault_code` should be set to 0 before calling.

   Upon returning, `u->fault_code` indicates the reason execution ended:
     0: Instruction limit was reached before executing a halt instruction.
        The program needs to execute more before it finishes.
     1: The program halted normally after executing a halt instruction.
     x: The program halted after an error was encountered.

   `u->ram` should point to a buffer 0x100001 bytes in size. The extra byte
   prevents 16-bit POKE and PEEK instructions with address 0xFFFF going out
   of bounds. Uxn does not wrap those high byte accesses to 0x0. */
unsigned int UxnExec(UxnCore *u, unsigned int limit);

#endif
