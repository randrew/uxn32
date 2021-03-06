#include "core32.h"

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
#define POP8(o) { if(!(j = *sp)) goto fault_2; o = (Uint16)src->dat[--j]; *sp = j; }
#define POP16(o) { if((j = *sp) <= 1) goto fault_2; o = src->dat[j - 1]; o += src->dat[j - 2] << 8; *sp = j - 2; }
#define POP(o) { if(bs) POP16(o) else POP8(o) }
#define POKE(x, y) { if(bs) { u->ram[(x)] = (y) >> 8; u->ram[((x) + 1) & 0xFFFFu] = (y); } else u->ram[(x)] = y; }
#define PEEK16(o, x) { o = (u->ram[(x)] << 8) + u->ram[((x) + 1) & 0xFFFFu]; }
#define PEEK(o, x) { if(bs) PEEK16(o, x) else o = u->ram[(x)]; }
#define DEVR(o, x) { o = u->dei(u, x); if (bs) o = (o << 8) + u->dei(u, ((x) + 1) & 0xFF); }
#define DEVW(x, y) { if (bs) { u->deo(u, (x), (y) >> 8); u->deo(u, ((x) + 1) & 0xFF, (y)); } else u->deo(u, x, (y)); }
#define WARP(x) { if(bs) pc = (x); else pc += (Sint8)(x); }

unsigned int
UxnExec(UxnCore *u, unsigned int limit)
{
	unsigned int a, b, c, j, k, bs, instr, pc;
	Uint8 kptr, *sp;
	UxnStack *src, *dst;
	pc = u->pc;
	while(limit) {
		limit--;
		instr = u->ram[pc];
		pc = (pc + 1) & 0xFFFFu;
		if (!instr) { u->fault_code = 1; goto done; }
		/* Return Mode */
		if(instr & 0x40) {
			src = u->rst; dst = u->wst;
		} else {
			src = u->wst; dst = u->rst;
		}
		/* Keep Mode */
		if(instr & 0x80) {
			kptr = src->ptr;
			sp = &kptr;
		} else {
			sp = &src->ptr;
		}
		/* Short Mode */
		bs = instr & 0x20 ? 1 : 0;
		switch(instr & 0x1F) {
		/* Stack */
		case 0x00: /* LIT */ if(bs) { PEEK16(a, pc) PUSH16(src, a) pc += 2; }
		                     else   { a = u->ram[pc]; PUSH8(src, a) pc++; } break;
		case 0x01: /* INC */ POP(a) PUSH(src, a + 1) break;
		case 0x02: /* POP */ POP(a) break;
		case 0x03: /* NIP */ POP(a) POP(b) PUSH(src, a) break;
		case 0x04: /* SWP */ POP(a) POP(b) PUSH(src, a) PUSH(src, b) break;
		case 0x05: /* ROT */ POP(a) POP(b) POP(c) PUSH(src, b) PUSH(src, a) PUSH(src, c) break;
		case 0x06: /* DUP */ POP(a) PUSH(src, a) PUSH(src, a) break;
		case 0x07: /* OVR */ POP(a) POP(b) PUSH(src, b) PUSH(src, a) PUSH(src, b) break;
		/* Logic */
		case 0x08: /* EQU */ POP(a) POP(b) PUSH8(src, b == a) break;
		case 0x09: /* NEQ */ POP(a) POP(b) PUSH8(src, b != a) break;
		case 0x0A: /* GTH */ POP(a) POP(b) PUSH8(src, b > a) break;
		case 0x0B: /* LTH */ POP(a) POP(b) PUSH8(src, b < a) break;
		case 0x0C: /* JMP */ POP(a) WARP(a) break;
		case 0x0D: /* JCN */ POP(a) POP8(b) if(b) WARP(a) break;
		case 0x0E: /* JSR */ POP(a) PUSH16(dst, pc) WARP(a) break;
		case 0x0F: /* STH */ POP(a) PUSH(dst, a) break;
		/* Memory */
		case 0x10: /* LDZ */ POP8(a) PEEK(b, a) PUSH(src, b) break;
		case 0x11: /* STZ */ POP8(a) POP(b) POKE(a, b) break;
		case 0x12: /* LDR */ POP8(a) PEEK(b, pc + (Sint8)a) PUSH(src, b) break;
		case 0x13: /* STR */ POP8(a) POP(b) c = pc + (Sint8)a; POKE(c, b) break;
		case 0x14: /* LDA */ POP16(a) PEEK(b, a) PUSH(src, b) break;
		case 0x15: /* STA */ POP16(a) POP(b) POKE(a, b) break;
		case 0x16: /* DEI */ POP8(a) DEVR(b, a) PUSH(src, b) break;
		case 0x17: /* DEO */ POP8(a) POP(b) DEVW(a, b) if (u->fault_code) goto done; break;
		/* Arithmetic */
		case 0x18: /* ADD */ POP(a) POP(b) PUSH(src, b + a) break;
		case 0x19: /* SUB */ POP(a) POP(b) PUSH(src, b - a) break;
		case 0x1A: /* MUL */ POP(a) POP(b) PUSH(src, (unsigned int)b * a) break;
		case 0x1B: /* DIV */ POP(a) POP(b) if(a == 0) { u->fault_code = 4; goto done; } PUSH(src, b / a) break;
		case 0x1C: /* AND */ POP(a) POP(b) PUSH(src, b & a) break;
		case 0x1D: /* ORA */ POP(a) POP(b) PUSH(src, b | a) break;
		case 0x1E: /* EOR */ POP(a) POP(b) PUSH(src, b ^ a) break;
		case 0x1F: /* SFT */ POP8(a) POP(b) c = b >> (a & 0x0F) << ((a & 0xF0) >> 4); PUSH(src, c) break;
		}
	}
done:
	u->pc = pc;
	return limit;
fault_2: u->fault_code = 2; goto done;
fault_3: u->fault_code = 3; goto done;
}
