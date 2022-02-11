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
	x,y: macro in params. d: macro in device. j,k,dev: macro temp variables. o: macro out param. */

#define PUSH8(s, x) { if(s->ptr == 0xff) { errcode = 2; goto err; } s->dat[s->ptr++] = (x); }
#define PUSH16(s, x) { if((j = s->ptr) >= 0xfe) { errcode = 2; goto err; } k = (x); s->dat[j] = k >> 8; s->dat[j + 1] = k; s->ptr = j + 2; }
#define PUSH(s, x) { if(bs) { PUSH16(s, (x)) } else { PUSH8(s, (x)) } }
#define POP8(o) { if(!(j = *sp)) { errcode = 1; goto err; } o = (Uint16)src->dat[--j]; *sp = j; }
#define POP16(o) { if((j = *sp) <= 1) { errcode = 1; goto err; } o = src->dat[j - 1]; o += src->dat[j - 2] << 8; *sp = j - 2; }
#define POP(o) { if(bs) { POP16(o) } else { POP8(o) } }
#define POKE(x, y) { if(bs) { u->ram[(x)] = (y) >> 8; u->ram[(x) + 1] = (y); } else { u->ram[(x)] = y; } }
#define PEEK16(o, x) { o = (u->ram[(x)] << 8) + u->ram[(x) + 1]; }
#define PEEK(o, x) { if(bs) { PEEK16(o, x) } else { o = u->ram[(x)]; } }
#define DEVR(o, d, x) { dev = (d); o = dev->dei(dev, (x) & 0x0f); if(bs) { o = (o << 8) + dev->dei(dev, ((x) + 1) & 0x0f); } }
#define DEVW8(x, y) { dev->dat[(x) & 0xf] = y; dev->deo(dev, (x) & 0x0f); }
#define DEVW(d, x, y) { dev = (d); if(bs) { DEVW8((x), (y) >> 8); DEVW8((x) + 1, (y)); } else { DEVW8((x), (y)) } }
#define WARP(x) { if(bs) pc = (x); else pc += (Sint8)(x); }

unsigned int
UxnExec(Uxn *u, unsigned int limit)
{
	unsigned int a, b, c, j, k, bs, instr, errcode, pc;
	Uint8 kptr, *sp;
	Stack *src, *dst;
	Device *dev;
	pc = u->pc;
	if(u->dev[0].dat[0xf]) return limit;
	if(u->wst->ptr > 0xf8) u->wst->ptr = 0xf8;
	while(limit) {
		limit--;
		instr = u->ram[pc];
		pc = (pc + 1) & 0xFFFFu;
		if (!instr) break;
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
		switch(instr & 0x1f) {
		/* Stack */
		case 0x00: /* LIT */ if(bs) { PEEK16(a, pc) PUSH16(src, a) pc += 2; }
		                     else   { a = u->ram[pc]; PUSH8(src, a) pc++; } break;
		case 0x01: /* INC */ POP(a) PUSH(src, a + 1) break;
		case 0x02: /* POP */ POP(a) break;
		case 0x03: /* DUP */ POP(a) PUSH(src, a) PUSH(src, a) break;
		case 0x04: /* NIP */ POP(a) POP(b) PUSH(src, a) break;
		case 0x05: /* SWP */ POP(a) POP(b) PUSH(src, a) PUSH(src, b) break;
		case 0x06: /* OVR */ POP(a) POP(b) PUSH(src, b) PUSH(src, a) PUSH(src, b) break;
		case 0x07: /* ROT */ POP(a) POP(b) POP(c) PUSH(src, b) PUSH(src, a) PUSH(src, c) break;
		/* Logic */
		case 0x08: /* EQU */ POP(a) POP(b) PUSH8(src, b == a) break;
		case 0x09: /* NEQ */ POP(a) POP(b) PUSH8(src, b != a) break;
		case 0x0a: /* GTH */ POP(a) POP(b) PUSH8(src, b > a) break;
		case 0x0b: /* LTH */ POP(a) POP(b) PUSH8(src, b < a) break;
		case 0x0c: /* JMP */ POP(a) WARP(a) break;
		case 0x0d: /* JCN */ POP(a) POP8(b) if(b) WARP(a) break;
		case 0x0e: /* JSR */ POP(a) PUSH16(dst, pc) WARP(a) break;
		case 0x0f: /* STH */ POP(a) PUSH(dst, a) break;
		/* Memory */
		case 0x10: /* LDZ */ POP8(a) PEEK(b, a) PUSH(src, b) break;
		case 0x11: /* STZ */ POP8(a) POP(b) POKE(a, b) break;
		case 0x12: /* LDR */ POP8(a) PEEK(b, pc + (Sint8)a) PUSH(src, b) break;
		case 0x13: /* STR */ POP8(a) POP(b) c = pc + (Sint8)a; POKE(c, b) break;
		case 0x14: /* LDA */ POP16(a) PEEK(b, a) PUSH(src, b) break;
		case 0x15: /* STA */ POP16(a) POP(b) POKE(a, b) break;
		case 0x16: /* DEI */ POP8(a) DEVR(b, &u->dev[a >> 4], a) PUSH(src, b) break;
		case 0x17: /* DEO */ POP8(a) POP(b) DEVW(&u->dev[a >> 4], a, b) if (u->fault_code) goto done; break;
		/* Arithmetic */
		case 0x18: /* ADD */ POP(a) POP(b) PUSH(src, b + a) break;
		case 0x19: /* SUB */ POP(a) POP(b) PUSH(src, b - a) break;
		case 0x1a: /* MUL */ POP(a) POP(b) PUSH(src, (Uint32)b * a) break;
		case 0x1b: /* DIV */ POP(a) POP(b) if(a == 0) { errcode = 3; goto err; } PUSH(src, b / a) break;
		case 0x1c: /* AND */ POP(a) POP(b) PUSH(src, b & a) break;
		case 0x1d: /* ORA */ POP(a) POP(b) PUSH(src, b | a) break;
		case 0x1e: /* EOR */ POP(a) POP(b) PUSH(src, b ^ a) break;
		case 0x1f: /* SFT */ POP8(a) POP(b) c = b >> (a & 0x0f) << ((a & 0xf0) >> 4); PUSH(src, c) break;
		}
	}
done:
	u->pc = pc;
	return limit;
err:
	u->fault_code = errcode;
	goto done;
}
