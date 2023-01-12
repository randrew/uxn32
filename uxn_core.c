#include "uxn_core.h"

/*
Copyright (u) 2022 Devine Lu Linvega, Andrew Alderwick, Andrew Richards

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
*/

/* clang-format off */

/*	a,b,c: general use.  bs: byte/short bool. src, dst: stack ptrs, swapped in return mode.
	pc: program counter. sp: ptr to src stack ptr. kptr: "keep" mode copy of src stack ptr.
	x,y: macro in params. j,k: macro temp variables. o: macro out param. */

#define PUSH8(s, x) { if(s->ptr == 0xFF) goto fault_3; s->dat[s->ptr++] = (x); }
#define PUSH16(s, x) { if((j = s->ptr) >= 0xFE) goto fault_3; k = (x); s->dat[j] = k >> 8; s->dat[j + 1] = k; s->ptr = j + 2; }
#define PUSH(s, x) { if(bs) PUSH16(s, (x)) else PUSH8(s, (x)) }
#define POP8(o) { if(!(j = *sp)) goto fault_2; o = (UxnU16)src->dat[--j]; *sp = j; }
#define POP16(o) { if((j = *sp) <= 1) goto fault_2; o = src->dat[j - 1]; o += src->dat[j - 2] << 8; *sp = j - 2; }
#define POP(o) { if(bs) POP16(o) else POP8(o) }
#define POKE(x, y) { if(bs) { u->ram[(x)] = (y) >> 8; u->ram[(x) + 1] = (y); } else u->ram[(x)] = y; }
#define PEEK16(o, x) { o = (u->ram[(x)] << 8) + u->ram[(x) + 1]; }
#define PEEK(o, x) { if(bs) PEEK16(o, x) else o = u->ram[(x)]; }
#define DEVR(o, x) { o = u->dei(u, x); if (bs) o = (o << 8) + u->dei(u, ((x) + 1) & 0xFF); }
#define DEVW(x, y) { if(bs) { u->deo(u, (x), (y) >> 8); u->deo(u, ((x) + 1) & 0xFF, (y)); } else u->deo(u, x, (y)); }
#define WARP(x) { if(bs) pc = (x); else pc += (UxnI8)(x); }

#define DISPATCH(opcode, body) \
	case opcode|0x80|0x40|0x20: {enum{bs=1}; src = u->rst, dst = u->wst; kptr = src->ptr, sp = &kptr; body break;}\
	case opcode|0x80|0x40|0x00: {enum{bs=0}; src = u->rst, dst = u->wst; kptr = src->ptr, sp = &kptr; body break;}\
	case opcode|0x80|0x00|0x20: {enum{bs=1}; src = u->wst, dst = u->rst; kptr = src->ptr, sp = &kptr; body break;}\
	case opcode|0x80|0x00|0x00: {enum{bs=0}; src = u->wst, dst = u->rst; kptr = src->ptr, sp = &kptr; body break;}\
	case opcode|0x00|0x40|0x20: {enum{bs=1}; src = u->rst, dst = u->wst; sp = &src->ptr; body break;}\
	case opcode|0x00|0x40|0x00: {enum{bs=0}; src = u->rst, dst = u->wst; sp = &src->ptr; body break;}\
	case opcode|0x00|0x00|0x20: {enum{bs=1}; src = u->wst, dst = u->rst; sp = &src->ptr; body break;}\
	case opcode|0x00|0x00|0x00: {enum{bs=0}; src = u->wst, dst = u->rst; sp = &src->ptr; body break;}\


unsigned int
UxnExec(UxnCore *u, unsigned int limit)
{
	unsigned int a, b, c, j, k, instr, pc;
	UxnU8 kptr, *sp;
	UxnStack *src, *dst;
	pc = u->pc;
	while(limit) {
		limit--;
		instr = u->ram[pc];
		pc = (pc + 1) & 0xFFFFu;
		if (!instr) { u->fault_code = 1; goto done; } // TODO
		switch(instr) {
		// /* BRK */ case 0x00: u->fault_code = 1; goto done;
		DISPATCH(0x00, if(bs) { PEEK16(a, pc) PUSH16(src, a) pc += 2; } else { a = u->ram[pc]; PUSH8(src, a) pc++; } )
	/* Stack */
		/* INC */ DISPATCH(0x01, POP(a) PUSH(src, a + 1) )
		/* POP */ DISPATCH(0x02, POP(a) )
		/* NIP */ DISPATCH(0x03, POP(a) POP(b) PUSH(src, a) )
		/* SWP */ DISPATCH(0x04, POP(a) POP(b) PUSH(src, a) PUSH(src, b) )
		/* ROT */ DISPATCH(0x05, POP(a) POP(b) POP(c) PUSH(src, b) PUSH(src, a) PUSH(src, c) )
		/* DUP */ DISPATCH(0x06, POP(a) PUSH(src, a) PUSH(src, a) )
		/* OVR */ DISPATCH(0x07, POP(a) POP(b) PUSH(src, b) PUSH(src, a) PUSH(src, b) )
	/* Logic */
		/* EQU */ DISPATCH(0x08, POP(a) POP(b) PUSH8(src, b == a) )
		/* NEQ */ DISPATCH(0x09, POP(a) POP(b) PUSH8(src, b != a) )
		/* GTH */ DISPATCH(0x0A, POP(a) POP(b) PUSH8(src, b > a) )
		/* LTH */ DISPATCH(0x0B, POP(a) POP(b) PUSH8(src, b < a) )
		/* JMP */ DISPATCH(0x0C, POP(a) WARP(a) )
		/* JCN */ DISPATCH(0x0D, POP(a) POP8(b) if(b) WARP(a) )
		/* JSR */ DISPATCH(0x0E, POP(a) PUSH16(dst, pc) WARP(a) )
		/* STH */ DISPATCH(0x0F, POP(a) PUSH(dst, a) )
	/* Memory */
		/* LDZ */ DISPATCH(0x10, POP8(a) PEEK(b, a) PUSH(src, b) )
		/* STZ */ DISPATCH(0x11, POP8(a) POP(b) POKE(a, b) )
		/* LDR */ DISPATCH(0x12, POP8(a) PEEK(b, pc + (UxnI8)a) PUSH(src, b) )
		/* STR */ DISPATCH(0x13, POP8(a) POP(b) c = pc + (UxnI8)a; POKE(c, b) )
		/* LDA */ DISPATCH(0x14, POP16(a) PEEK(b, a) PUSH(src, b) )
		/* STA */ DISPATCH(0x15, POP16(a) POP(b) POKE(a, b) )
		/* DEI */ DISPATCH(0x16, POP8(a) DEVR(b, a) PUSH(src, b) )
		/* DEO */ DISPATCH(0x17, POP8(a) POP(b) DEVW(a, b) if (u->fault_code) goto done; )
	/* Arithmetic */
		/* ADD */ DISPATCH(0x18, POP(a) POP(b) PUSH(src, b + a) )
		/* SUB */ DISPATCH(0x19, POP(a) POP(b) PUSH(src, b - a) )
		/* MUL */ DISPATCH(0x1A, POP(a) POP(b) PUSH(src, (unsigned int)b * a) )
		/* DIV */ DISPATCH(0x1B, POP(a) POP(b) if(a == 0) { u->fault_code = 4; goto done; } PUSH(src, b / a) )
		/* AND */ DISPATCH(0x1C, POP(a) POP(b) PUSH(src, b & a) )
		/* ORA */ DISPATCH(0x1D, POP(a) POP(b) PUSH(src, b | a) )
		/* EOR */ DISPATCH(0x1E, POP(a) POP(b) PUSH(src, b ^ a) )
		/* SFT */ DISPATCH(0x1F, POP8(a) POP(b) c = b >> (a & 0x0F) << ((a & 0xF0) >> 4); PUSH(src, c) )
		}
	}
done:
	u->pc = pc;
	return limit;
fault_2: u->fault_code = 2; goto done;
fault_3: u->fault_code = 3; goto done;
}
