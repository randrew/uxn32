#ifndef __UXN_CORE32_H__
#define __UXN_CORE32_H__
/*
Copyright (c) 2022 Devine Lu Linvega, Andrew Richards

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
*/
#pragma warning(disable:4244) /* Noisy VC6 warning. Can't disable with flag */
typedef unsigned char Uint8;
typedef signed char Sint8;
typedef unsigned short Uint16;
typedef unsigned int Uint32;

typedef struct {
	Uint8 ptr, dat[255];
} Stack;

typedef struct Uxn {
	Uint8 *ram;
	Stack *wst, *rst;
	Uint8 (*dev_read)(struct Uxn *u, unsigned int address);
	void (*dev_write)(struct Uxn *u, unsigned int address, unsigned int value);
	Uint16 pc, fault_code;
} Uxn;

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
     x: The program halted after an error was encountered. */
unsigned int UxnExec(Uxn *u, unsigned int limit);

#endif
