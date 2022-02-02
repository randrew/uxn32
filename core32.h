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
typedef signed short Sint16;
typedef unsigned int Uint32;

typedef struct {
	Uint8 ptr, dat[255];
} Stack;

typedef struct Device {
	struct Uxn *u;
	Uint8 *dat;
	Uint8 (*dei)(struct Device *d, Uint8);
	void (*deo)(struct Device *d, Uint8);
} Device;

typedef struct Uxn {
	Uint8 *ram;
	Stack *wst, *rst;
	Device dev[16];
	Uint16 pc, fault_code;
} Uxn;

unsigned int UxnExec(Uxn *u, unsigned int limit);

#endif
