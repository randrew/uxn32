#include "uxn_core.h"

/*
Copyright (c) 2022-2023 Devine Lu Linvega, Andrew Alderwick, Andrew Richards

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE.
*/

/*	a,b,c: general use.  bs: byte/short bool. src, dst: stack ptrs, swapped in return mode.
	pc: program counter. snum: pointer to src stack num. knum: "keep mode" copy of src stack num.
	x,y: macro in params. j,k: macro temp variables. o: macro out param. */

#define PUSH8(s, x) { s->mem[j = s->num] = (x); s->num = j + 1 & 0xFF; }
#define PUSH16(s, x) { j = s->num; k = (x); s->mem[j] = k >> 8; s->mem[j + 1 & 0xFF] = k; s->num = j + 2 & 0xFF; }
#define PUSH(s, x) { if(bs) PUSH16(s, (x)) else PUSH8(s, (x)) }
#define POP8(o) { o = src->mem[*snum = *snum - 1 & 0xFF]; }
#define POP16(o) { j = *snum; o = src->mem[j - 1 & 0xFF]; o += src->mem[j = j - 2 & 0xFF] << 8; *snum = j; }
#define POP(o) { if(bs) POP16(o) else POP8(o) }
#define POKE(x, y) { if(bs) { u->ram[(x)] = (y) >> 8; u->ram[(x) + 1] = (y); } else u->ram[(x)] = y; }
#define PEEK16(o, x) { o = (u->ram[(x)] << 8) + u->ram[(x) + 1]; }
#define PEEK(o, x) { if(bs) PEEK16(o, x) else o = u->ram[(x)]; }
#define DEVR(o, x) { o = u->dei(u, x); if(bs) o = (o << 8) + u->dei(u, ((x) + 1) & 0xFF); }
#define DEVW(x, y) { if(bs) { u->deo(u, (x), (y) >> 8); u->deo(u, ((x) + 1) & 0xFF, (y)); } else u->deo(u, x, (y)); }
#define JUMP(x) { if(bs) pc = (x); else pc += (UxnI8)(x); }

#define MODE(op, body)\
	case 0x00|0x00|0x00|op: {enum{bs=0}; src = u->wst, dst = u->rst; snum = &src->num; body break;}\
	case 0x00|0x00|0x20|op: {enum{bs=1}; src = u->wst, dst = u->rst; snum = &src->num; body break;}\
	case 0x00|0x40|0x00|op: {enum{bs=0}; src = u->rst, dst = u->wst; snum = &src->num; body break;}\
	case 0x00|0x40|0x20|op: {enum{bs=1}; src = u->rst, dst = u->wst; snum = &src->num; body break;}\
	case 0x80|0x00|0x00|op: {enum{bs=0}; src = u->wst, dst = u->rst; knum = src->num, snum = &knum; body break;}\
	case 0x80|0x00|0x20|op: {enum{bs=1}; src = u->wst, dst = u->rst; knum = src->num, snum = &knum; body break;}\
	case 0x80|0x40|0x00|op: {enum{bs=0}; src = u->rst, dst = u->wst; knum = src->num, snum = &knum; body break;}\
	case 0x80|0x40|0x20|op: {enum{bs=1}; src = u->rst, dst = u->wst; knum = src->num, snum = &knum; body break;}

unsigned int
UxnExec(UxnCore *u, unsigned int limit)
{
	unsigned int a, b, c, j, k; UxnU16 pc = u->pc;
	UxnU8 knum, *snum; UxnStack *src, *dst;
	while(limit) {
		limit--;
		switch(u->ram[pc++]) {
		/* BRK */ case 0x00: u->fault = 1; goto done;
		/* JCI */ case 0x20: snum = &u->wst->num, src = u->wst; POP8(b) if(b) goto JMI; pc += 2; break;
		/* JMI */ case 0x40: JMI: PEEK16(a, pc) pc += a + 2; break;
		/* JSI */ case 0x60: PUSH16(u->rst, pc + 2) goto JMI;
		/* LIT */ case 0x80: a = u->ram[pc++]; PUSH8(u->wst, a); break;
		          case 0xA0: PEEK16(a, pc) PUSH16(u->wst, a) pc += 2; break;
		          case 0xC0: a = u->ram[pc++]; PUSH8(u->rst, a); break;
		          case 0xE0: PEEK16(a, pc) PUSH16(u->rst, a) pc += 2; break;
		/* INC */ MODE(0x01, POP(a) PUSH(src, a + 1))
		/* POP */ MODE(0x02, POP(a))
		/* NIP */ MODE(0x03, POP(a) POP(b) PUSH(src, a))
		/* SWP */ MODE(0x04, POP(a) POP(b) PUSH(src, a) PUSH(src, b))
		/* ROT */ MODE(0x05, POP(a) POP(b) POP(c) PUSH(src, b) PUSH(src, a) PUSH(src, c))
		/* DUP */ MODE(0x06, POP(a) PUSH(src, a) PUSH(src, a))
		/* OVR */ MODE(0x07, POP(a) POP(b) PUSH(src, b) PUSH(src, a) PUSH(src, b))
		/* EQU */ MODE(0x08, POP(a) POP(b) PUSH8(src, b == a))
		/* NEQ */ MODE(0x09, POP(a) POP(b) PUSH8(src, b != a))
		/* GTH */ MODE(0x0A, POP(a) POP(b) PUSH8(src, b > a))
		/* LTH */ MODE(0x0B, POP(a) POP(b) PUSH8(src, b < a))
		/* JMP */ MODE(0x0C, POP(a) JUMP(a))
		/* JCN */ MODE(0x0D, POP(a) POP8(b) if(b) JUMP(a))
		/* JSR */ MODE(0x0E, POP(a) PUSH16(dst, pc) JUMP(a))
		/* STH */ MODE(0x0F, POP(a) PUSH(dst, a))
		/* LDZ */ MODE(0x10, POP8(a) PEEK(b, a) PUSH(src, b))
		/* STZ */ MODE(0x11, POP8(a) POP(b) POKE(a, b))
		/* LDR */ MODE(0x12, POP8(a) b = pc + (UxnI8)a & 0xFFFF; PEEK(c, b) PUSH(src, c))
		/* STR */ MODE(0x13, POP8(a) POP(b) c = pc + (UxnI8)a & 0xFFFF; POKE(c, b))
		/* LDA */ MODE(0x14, POP16(a) PEEK(b, a) PUSH(src, b))
		/* STA */ MODE(0x15, POP16(a) POP(b) POKE(a, b))
		/* DEI */ MODE(0x16, POP8(a) DEVR(b, a) PUSH(src, b))
		/* DEO */ MODE(0x17, POP8(a) POP(b) DEVW(a, b) if(u->fault) goto done;)
		/* ADD */ MODE(0x18, POP(a) POP(b) PUSH(src, b + a))
		/* SUB */ MODE(0x19, POP(a) POP(b) PUSH(src, b - a))
		/* MUL */ MODE(0x1A, POP(a) POP(b) PUSH(src, b * a))
		/* DIV */ MODE(0x1B, POP(a) POP(b) if(!a) { u->fault = 4; goto done; } PUSH(src, b / a))
		/* AND */ MODE(0x1C, POP(a) POP(b) PUSH(src, b & a))
		/* ORA */ MODE(0x1D, POP(a) POP(b) PUSH(src, b | a))
		/* EOR */ MODE(0x1E, POP(a) POP(b) PUSH(src, b ^ a))
		/* SFT */ MODE(0x1F, POP8(a) POP(b) c = b >> (a & 0x0F) << ((a & 0xF0) >> 4); PUSH(src, c))
		}
	}
done: u->pc = pc; return limit;
}
