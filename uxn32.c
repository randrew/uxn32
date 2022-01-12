#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include "resource.h"
#include <windows.h>
#include <shlwapi.h>

#if !defined(_WIN64) && _WINVER < 0x0500
#define GetWindowLongPtrA   GetWindowLongA
#define GetWindowLongPtrW   GetWindowLongW
#define SetWindowLongPtrA   SetWindowLongA
#define SetWindowLongPtrW   SetWindowLongW
typedef LONG LONG_PTR;
#define GWLP_USERDATA GWL_USERDATA
#ifdef UNICODE
#define GetWindowLongPtr  GetWindowLongPtrW
#define SetWindowLongPtr  SetWindowLongPtrW
#else
#define GetWindowLongPtr  GetWindowLongPtrA
#define SetWindowLongPtr  SetWindowLongPtrA
#endif // !UNICODE
#endif

#ifndef INVALID_FILE_ATTRIBUTES
#define INVALID_FILE_ATTRIBUTES ((DWORD)-1)
#endif
#ifndef WM_MOUSEWHEEL
#define WM_MOUSEWHEEL 0x020A
#endif
#ifndef WM_MOUSELEAVE
#define WM_MOUSELEAVE 0x02A3
#endif
#ifndef MAPVK_VK_TO_CHAR
#define MAPVK_VK_TO_CHAR    (2)
#endif

#pragma comment(lib, "user32.lib")
#pragma comment(lib, "gdi32.lib")
#pragma comment(lib, "Shlwapi.lib")

#include "core32.h"
// #include "devices/audio.h"

#define OFFSET_OF(s, m) ((SIZE_T)&(((s*)0)->m))
#define OUTER_OF(outer, type, field) ((type *) ((char *)(outer) - OFFSET_OF(type, field)))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#define MIN(a, b) (((a) < (b)) ? (a) : (b))

#define UXN_DEFAULT_WIDTH (64 * 8)
#define UXN_DEFAULT_HEIGHT (40 * 8)
#define UXN_RAM_SIZE (0x10000u)
#define UXN_ROM_OFFSET (0x0100u)

#define DEVPEEK16(d, o, x) { (o) = ((d)->dat[(x)] << 8) + (d)->dat[(x) + 1]; }
#define DEVPOKE16(d, a, v) { (d)->dat[(a)] = (v) >> 8; (d)->dat[(a) + 1] = (v); }
#define DEVPOKE16X2(d, a, v1, v2) { DEVPOKE16(d, a, v1) DEVPOKE16(d, a + 2, v2) }
/* ^ TODO remove if unused */
#define GETVECTOR(d) ((d)->dat[0] << 8 | (d)->dat[1])

typedef struct UxnBox {
	void *user;
	Uxn core;
	Device *dev_system, *dev_screen, *dev_mouse, *dev_ctrl, *dev_audio0, *dev_console;
	Stack work_stack, ret_stack;
	Uint8 device_memory[256];
} UxnBox;

typedef struct UxnScreen {
	Uint32 palette[4];
	LONG width, height;
	Uint8 *fg, *bg;
} UxnScreen;

typedef struct UxnFiler {
	HANDLE hFile, hFind;
	TCHAR path[MAX_PATH];
	DWORD pathlen;
	WIN32_FIND_DATA find_data;
	enum
	{
		FileDevState_Init,
		FileDevState_Reading,
		FileDevState_Writing
	} state;
} UxnFiler;

typedef struct EmuWindow {
	UxnBox *box;
	HBITMAP hBMP;
	HDC hDibDC;
	UINT needs_clear : 1;
	BOOL host_cursor;

	SIZE dib_dims;
	UxnScreen screen; // could move to a UxnGrafxBox
	UxnFiler filer;
} EmuWindow;

static Uint8
nil_dei(Device *d, Uint8 port)
{ return d->dat[port]; }
static void
nil_deo(Device *d, Uint8 port)
{ (void)d;(void)port; }

static void
DebugPrint(char const *fmt, ...)
{
	va_list ap; int res; char buffer[1024 + 1];
	va_start(ap, fmt);
	res = wvsprintfA(buffer, fmt, ap);
	va_end(ap);
	if (res < 0 || res >= 1024) return;
	buffer[res] = '\n';
	buffer[res + 1] = 0;
	OutputDebugStringA(buffer);
}

static void
PrintLastError(void)
{
	DWORD dw = GetLastError();
	LPVOID lpMsgBuf;
	FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, dw, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR) &lpMsgBuf, 0, NULL );
	DebugPrint("Win32 error %d: %s", (int)dw, lpMsgBuf);
}

static void
VFmtBox(LPCSTR title, UINT flags, char const *fmt, va_list ap)
{
	int res; char buffer[1024 + 1];
	res = wvsprintfA(buffer, fmt, ap);
	if (res < 0 || res >= 1024) return;
	buffer[res] = '\n';
	buffer[res + 1] = 0;
	MessageBox(0, buffer, title, flags);
}

static void
DebugBox(char const *fmt, ...)
{
	va_list ap; va_start(ap, fmt);
	VFmtBox(TEXT("Debug"), MB_OK, fmt, ap);
	va_end(ap);
}

static __declspec(noreturn) void
FatalBox(char const *fmt, ...)
{
	va_list ap; va_start(ap, fmt);
	while (ShowCursor(TRUE) < 0);
	VFmtBox(TEXT("Major Problem"), MB_OK | MB_ICONSTOP | MB_TASKMODAL, fmt, ap);
	va_end(ap);
	ExitProcess(ERROR_GEN_FAILURE);
}

static __declspec(noreturn) void
OutOfMemory(void)
{
	MessageBox(NULL, TEXT("Out of memory"), NULL, MB_OK | MB_ICONSTOP | MB_TASKMODAL);
	ExitProcess(ERROR_OUTOFMEMORY);
}

static void * AllocZeroedOrFail(SIZE_T bytes)
{
	void *result = HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, bytes);
	if (!result) OutOfMemory();
	return result;
}

#if 0
static char *
LoadEntireFile(LPCSTR path)
{
	DWORD bytes_read; char *result = NULL;
	BY_HANDLE_FILE_INFORMATION info;
	HANDLE hFile = CreateFile(path, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hFile == INVALID_HANDLE_VALUE) return NULL;
	if (!GetFileInformationByHandle(hFile, &info)) goto done;
	if (info.nFileSizeHigh) goto done;
	result = HeapAlloc(GetProcessHeap(), 0, info.nFileSizeLow);
	if (!result) OutOfMemory();
	if (!ReadFile(hFile, result, info.nFileSizeLow, &info.nFileSizeLow, &bytes_read))
		FatalBox("Read error while reading file %s", path);
done:
	CloseHandle(hFile);
	return result;
}
#endif

static BOOL
LoadFileInto(LPCSTR path, char *dest, DWORD max_bytes, DWORD *bytes_read)
{
	HANDLE hFile = CreateFile(path, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hFile == INVALID_HANDLE_VALUE) return FALSE;
	if (!ReadFile(hFile, dest, max_bytes, bytes_read, NULL))
		FatalBox("Read error while reading file %s", path);
	CloseHandle(hFile);
	return TRUE;
}


int
uxn_halt(Uxn *u, Uint8 error, Uint16 addr)
{
	static char const * const errors[] = {
		"Working-stack underflow",
		"Return-stack underflow",
		"Working-stack overflow",
		"Return-stack overflow",
		"Working-stack division by zero",
		"Return-stack division by zero"};
	DebugBox("Uxn machine halted: %s#%04x, at 0x%04x\n", errors[error], u->ram[addr], addr);
	return 0;
}

int uxn_eval(Uxn *u, unsigned int pc)
{
	u->pc = pc;
	/* TODO there's something fancy we should do with the loop to make it tell if it ran out or not by return value, returning 0 when limit is 0 means we might have succeeded in reaching the null instruction on the last allowed step, so we need to do something else */
	if (UxnExec(u, 100000000) == 0) FatalBox("Uxn machine took too long");
	if (u->fault_code) DebugBox("Uxn machine faulted: %d, at %dx", (int)u->fault_code, (int)u->pc);
	return 1;
}

/* TODO try to eval uxn, if not, put into queue. in the one that's working, keep re-running and then give time to message loop? */

static Uint8 SpriteBlendingTable[5][16] = {
	{0, 0, 0, 0, 1, 0, 1, 1, 2, 2, 0, 2, 3, 3, 3, 0},
	{0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3},
	{1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1},
	{2, 3, 1, 2, 2, 3, 1, 2, 2, 3, 1, 2, 2, 3, 1, 2},
	{1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0}};

static void
DrawUxnSprite(UxnScreen *p, Uint8 *layer_pixels, Uint16 x, Uint16 y, Uint8 *sprite, Uint8 color, Uint8 flipx, Uint8 flipy, Uint8 twobpp)
{
	int v, h, opaque = SpriteBlendingTable[4][color], width = p->width, height = p->height;
	for (v = 0; v < 8; v++)
	{
		Uint16 c = sprite[v] | (twobpp ? sprite[v + 8] : 0) << 8;
		for (h = 7; h >= 0; --h, c >>= 1)
		{
			Uint8 ch = (c & 1) | ((c >> 7) & 2);
			if (opaque || ch)
			{
				int x0 = x + (flipx ? 7 - h : h), y0 = y + (flipy ? 7 - v : v);
				if (x0 < width && y0 < height)
					layer_pixels[x0 + y0 * width] = SpriteBlendingTable[ch][color];
			}
		}
	}
}


static void SetUxnScreenSize(UxnScreen *p, DWORD width, DWORD height)
{
	DWORD one_size = width * height, two_size = one_size * 2;
	HANDLE hHeap = GetProcessHeap(); BOOL reallocing = p->bg != NULL;
	// TODO pad for simd
	Uint8 *buff = reallocing ? HeapReAlloc(hHeap, 0, p->bg, two_size) : HeapAlloc(hHeap, HEAP_ZERO_MEMORY, two_size);
	if (!buff) OutOfMemory();
	if (reallocing) ZeroMemory(buff, two_size);
	p->bg = buff;
	p->fg = buff + one_size;
	p->width = width;
	p->height = height;
}
static void FreeUxnScreen(UxnScreen *p)
{
	if (p->bg) HeapFree(GetProcessHeap(), 0, p->bg);
}

UxnScreen * ScreenOfDevice(Device *d)
{
	UxnBox *box = OUTER_OF(d->u, UxnBox, core);
	return &((EmuWindow *)box->user)->screen;
}

Uint8
ScreenDevInCb(Device *d, Uint8 port)
{
	UxnScreen *screen = ScreenOfDevice(d);
	switch(port)
	{
	case 0x2: return screen->width >> 8;
	case 0x3: return screen->width;
	case 0x4: return screen->height >> 8;
	case 0x5: return screen->height;
	case 0x6:
	default: return d->dat[port];
	}
}

void
ScreenDevOutCb(Device *d, Uint8 port)
{
	UxnScreen *screen = ScreenOfDevice(d);
	switch(port) {
	case 0x5:
	{
		DWORD w, h;
		DEVPEEK16(d, w, 0x2) DEVPEEK16(d, h, 0x4)
		if (w > 1024 || h > 1024)
		{
			/* If the size is unacceptable, write back the old one */
			DEVPOKE16(d, 0x2, screen->width)
			DEVPOKE16(d, 0x4, screen->height)
		}
		else SetUxnScreenSize(screen, w, h);
		break;
	}
	case 0xe:
	{
		Uint16 x, y;
		Uint8 layer = d->dat[0xe] & 0x40, *pixels = layer ? screen->fg : screen->bg;
		int width = screen->width;
		DEVPEEK16(d, x, 0x8);
		DEVPEEK16(d, y, 0xa);
		if (x < width && y < screen->height) /* poke pixel */
			pixels[x + y * width] = d->dat[0xe] & 0x3;
		if(d->dat[0x6] & 0x01) DEVPOKE16(d, 0x8, x + 1); /* auto x+1 */
		if(d->dat[0x6] & 0x02) DEVPOKE16(d, 0xa, y + 1); /* auto y+1 */
		break;
	}
	case 0xf:
	{
		UINT x, y, addr, tmp, advnc = d->dat[0x6], sprite = d->dat[0xf];
		UINT twobpp = !!(sprite & 0x80);
		Uint8 *layer_pixels = (sprite & 0x40) ? screen->fg : screen->bg;
		DEVPEEK16(d, x, 0x8);
		DEVPEEK16(d, y, 0xa);
		DEVPEEK16(d, addr, 0xc);
		DrawUxnSprite(screen, layer_pixels, x, y, &d->mem[addr], sprite & 0xf, sprite & 0x10, sprite & 0x20, twobpp);
		/* auto addr+length */
		if(advnc & 0x04) { tmp = addr + 8 + twobpp * 8; DEVPOKE16(d, 0xc, tmp); }
		if(advnc & 0x01) { tmp = x + 8; DEVPOKE16(d, 0x8, tmp); } /* auto x+8 */
		if(advnc & 0x02) { tmp = y + 8; DEVPOKE16(d, 0xa, tmp); } /* auto y+8 */
		break;
	}
	}
}

Uint8
SystemDevInCb(Device *d, Uint8 port)
{
	switch (port)
	{
	case 0x2: return d->u->wst->ptr;
	case 0x3: return d->u->rst->ptr;
	}
	return d->dat[port];
}

void
SystemDevOutCb(Device *d, Uint8 port)
{
	switch (port)
	{
	case 0x2: d->u->wst->ptr = d->dat[port]; return;
	case 0x3: d->u->rst->ptr = d->dat[port]; return;
	}

	if (port > 0x7 && port < 0xe) /* modify palette */
	{
		UxnScreen *p = ScreenOfDevice(d);
		Uint8* addr = &d->dat[0x8];
		int i, shift;
		for(i = 0, shift = 4; i < 4; ++i, shift ^= 4)
		{
			Uint8
				r = (addr[0 + i / 2] >> shift) & 0x0f,
				g = (addr[2 + i / 2] >> shift) & 0x0f,
				b = (addr[4 + i / 2] >> shift) & 0x0f;
			p->palette[i] = 0x0f000000 | r << 16 | g << 8 | b;
			p->palette[i] |= p->palette[i] << 4;
		}
	}
}

static Uint8 SystemDevDateInCb(Device *d, Uint8 port)
{
	SYSTEMTIME t; TIME_ZONE_INFORMATION zone;
	GetLocalTime(&t);
	switch (port)
	{
	case 0x0: return (t.wYear + 1900) >> 8;
	case 0x1: return (t.wYear + 1900);
	case 0x2: return t.wMonth;
	case 0x3: return t.wDay;
	case 0x4: return t.wHour;
	case 0x5: return t.wMinute;
	case 0x6: return t.wSecond;
	case 0x7: return t.wDayOfWeek;
	case 0x8: /* Nth day of year doesn't seem readily available in Win32 */
	case 0x9: return 0;
	case 0xa: return GetTimeZoneInformation(&zone) == 2;
	}
	return d->dat[port];
}

/*********************************************************/


static void
ResetFiler(UxnFiler *f)
{
	if (f->hFile != INVALID_HANDLE_VALUE) { CloseHandle(f->hFile); f->hFile = INVALID_HANDLE_VALUE; }
	if (f->hFind != INVALID_HANDLE_VALUE) { FindClose(f->hFind); f->hFind = INVALID_HANDLE_VALUE; }
	f->state = FileDevState_Init;
}

static UxnFiler *
FilerOfDevice(Device *d)
{
	UxnBox *box = OUTER_OF(d->u, UxnBox, core);
	return &((EmuWindow *)box->user)->filer;
}

/* TODO error when file size beyond DWORD */

/* Returns 0 on error, including not enough space. Will not write to the dst buffer on error.
 * dst_len should be at least strlen(display_name) + 7 or it will definitely not work. */
static DWORD
PrintDirListRow(char *dst, DWORD dst_len, char *display_name, DWORD file_size, DWORD is_dir)
{
	int written; int ok = file_size < 0x10000; char tmp[1024 + 1];
	if (dst_len > sizeof tmp) dst_len = sizeof tmp;
	if (is_dir || !ok) written = wsprintfA(tmp, ok ? "---- %s\n" : "???? %s\n", display_name);
	else written = wsprintfA(tmp, "%04x %s\n", (unsigned int)file_size, display_name);
	if (written <= 0 || written >= (int)dst_len - 1) return 0;
	CopyMemory(dst, tmp, (DWORD)written + 1);
	return (DWORD)written;
}

static void
FileDevPathChange(Device *d)
{
	DWORD addr, i, avail;
	char tmp[MAX_PATH + 1], *in_mem;
	UxnFiler *f = FilerOfDevice(d);
	ResetFiler(f);
	DEVPEEK16(d, addr, 0x8);
	avail = UXN_RAM_SIZE - addr;
	in_mem = (char *)d->mem + addr;
	if (avail > MAX_PATH) avail = MAX_PATH;
	for (i = 0;; i++) {
		if (i >= avail) goto error;
		if (!(tmp[i] = in_mem[i])) break;
	}
	f->pathlen = GetFullPathNameA(tmp, MAX_PATH, f->path, NULL);
	if (f->pathlen == 0 || f->pathlen >= MAX_PATH) goto error;
	i = GetCurrentDirectoryA(MAX_PATH, tmp);
	if (i == 0 || i >= MAX_PATH) goto error;
	if (!PathIsPrefixA(tmp, f->path)) goto error;
	return;
error:
	f->path[0] = 0;
	f->pathlen = 0;
}

static DWORD
FileDevRead(UxnFiler *f, char *dst, DWORD dst_len)
{
	DWORD result = 0; DWORD const pathlen = f->pathlen; char *filepath = f->path;
	WIN32_FIND_DATA *find_data = &f->find_data;
	if (f->state != FileDevState_Reading) {
		DWORD attribs;
		ResetFiler(f);
		if ((attribs = GetFileAttributes(filepath)) == INVALID_FILE_ATTRIBUTES) return 0;
		if (attribs & FILE_ATTRIBUTE_DIRECTORY)
		{
			if (pathlen >= MAX_PATH - 3) return 0;
			filepath[pathlen] = '\\';
			filepath[pathlen + 1] = '*';
			filepath[pathlen + 2] = 0;
			f->hFind = FindFirstFile(filepath, find_data);
			filepath[pathlen] = 0;
			if (f->hFind == INVALID_HANDLE_VALUE) return 0;
		} else
		{
			f->hFile = CreateFile(filepath, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
			if (f->hFile == INVALID_HANDLE_VALUE) return 0;
		}
		f->state = FileDevState_Reading;
	}
	if (f->hFind != INVALID_HANDLE_VALUE) {
		for (;;) {
			// DWORD copy = WideCharToMultiByte(1252, 0, )
			DWORD written;
			if (find_data->cFileName[0] == '.' && find_data->cFileName[1] == 0) goto next;
			written = PrintDirListRow(dst, dst_len, find_data->cFileName, find_data->nFileSizeLow, find_data->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
			if (!written) break;
			dst += written; result += written; dst_len -= written;
		next:
			if (!FindNextFile(f->hFind, find_data)) goto done_dir;
		}
		if (result == 0) done_dir: ResetFiler(f); /* If 0 bytes were written, always end iteration */
	}
	else if (f->hFile != INVALID_HANDLE_VALUE) {
		DWORD bytes_read = dst_len;
		if (!ReadFile(f->hFile, dst, bytes_read, &bytes_read, NULL)) bytes_read = 0;
		if (bytes_read < dst_len) ResetFiler(f);
		result = bytes_read;
	}
	return result;
}

static DWORD
FileDevWrite(UxnFiler *f, char *src, DWORD src_len, int flags)
{
	DWORD written;
	if (f->state != FileDevState_Writing) {
		int append = flags & 0x01;
		ResetFiler(f);
		f->hFile = CreateFile(f->path, GENERIC_WRITE, FILE_SHARE_READ, NULL, append ? OPEN_ALWAYS : CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	}
	if (f->hFile == INVALID_HANDLE_VALUE) return 0;
	if (!WriteFile(f->hFile, src, src_len, &written, NULL)) {
		ResetFiler(f);
		return 0;
	}
	return written;
}

static DWORD
FileDevStat(UxnFiler *f, char *dst, DWORD dst_len)
{
	DWORD written; char path_tmp[MAX_PATH]; BY_HANDLE_FILE_INFORMATION info; BOOL ok, dir;
	f->hFile = CreateFile(f->path, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (f->hFile == INVALID_HANDLE_VALUE) return 0;
	ok = GetFileInformationByHandle(f->hFile, &info);
	dir = ok && info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY;
	CopyMemory(path_tmp, f->path, f->pathlen + 1);
	PathStripPathA(path_tmp);
	written = PrintDirListRow(dst, dst_len, path_tmp, ok ? info.nFileSizeLow : (DWORD)-1, dir);
	ResetFiler(f);
	return written;
}

static DWORD
FileDevDelete(UxnFiler *f)
{
	DWORD result = 0;
	ResetFiler(f);
	if (!f->pathlen) return 0;
	if (PathIsDirectoryA(f->path))
		result = RemoveDirectoryA(f->path);
	else
		result = DeleteFileA(f->path);
	return result ? 0 : 1;
}

Device *
uxn_port(Uxn *u, Uint8 id, Uint8 (*deifn)(Device *d, Uint8 port), void (*deofn)(Device *d, Uint8 port))
{
	Device *d = &u->dev[id];
	d->u = u;
	d->mem = u->ram; /* TODO unnecessary */
	d->dei = deifn;
	d->deo = deofn;
	d->dat = u->devpage + id * 0x10;
	return d;
}

void
file_deo(Device *d, Uint8 port)
{
	DWORD result = 0, /* next inits suppress msvc warning */ out_len = 0; char *out = 0;
	UxnFiler *f = FilerOfDevice(d);
	switch(port) { /* These need write location and size */
	int peek_at; DWORD dst, avail;
	case 0x5: peek_at = 0x4; goto calc;
	case 0xd: peek_at = 0xc; goto calc;
	case 0xf: peek_at = 0xe; goto calc;
	calc:
		DEVPEEK16(d, dst, peek_at);
		DEVPEEK16(d, out_len, 0xa);
		avail = UXN_RAM_SIZE - dst;
		if (out_len > avail) out_len = avail;
		out = (char *)d->mem + dst;
	}
	switch(port) {
	case 0x5: result = FileDevStat(f, out, out_len); goto result;
	case 0x6: result = FileDevDelete(f); goto result;
	case 0x9: result = 0; FileDevPathChange(d); goto result;
	case 0xd: result = FileDevRead(f, out, out_len); goto result;
	case 0xf: result = FileDevWrite(f, out, out_len, d->dat[0x7]); goto result;
	}
	return;
result:
	DEVPOKE16(d, 0x2, result);
}


/*********************************************************/


#define console_deo nil_deo
#define audio_dei nil_dei
#define audio_deo nil_deo

UxnBox *
MakeUxnBox(void)
{
	char *main_ram;
	UxnBox *box = (UxnBox *)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(UxnBox) + UXN_RAM_SIZE);
	if (!box) OutOfMemory();
	main_ram = (char *)(box + 1);
	box->core.ram = (Uint8 *)main_ram;
	box->core.devpage = box->device_memory;
	box->core.wst = &box->work_stack;
	box->core.rst = &box->ret_stack;
	/* system   */ box->dev_system = uxn_port(&box->core, 0x0, SystemDevInCb, SystemDevOutCb);
	/* console  */ box->dev_console = uxn_port(&box->core, 0x1, nil_dei, console_deo); /* ask if this should be shown in a console window on win32 and whether or not uxn roms tend to just passively poop out stuff in the background */
	/* screen   */ box->dev_screen = uxn_port(&box->core, 0x2, ScreenDevInCb, ScreenDevOutCb);
	/* audio0   */ box->dev_audio0 = uxn_port(&box->core, 0x3, audio_dei, audio_deo);
	/* audio1   */ uxn_port(&box->core, 0x4, audio_dei, audio_deo);
	/* audio2   */ uxn_port(&box->core, 0x5, audio_dei, audio_deo);
	/* audio3   */ uxn_port(&box->core, 0x6, audio_dei, audio_deo);
	/* unused   */ uxn_port(&box->core, 0x7, nil_dei, nil_deo);
	/* control  */ box->dev_ctrl = uxn_port(&box->core, 0x8, nil_dei, nil_deo);
	/* mouse    */ box->dev_mouse = uxn_port(&box->core, 0x9, nil_dei, nil_deo);
	/* file     */ uxn_port(&box->core, 0xa, nil_dei, file_deo);
	/* datetime */ uxn_port(&box->core, 0xb, SystemDevDateInCb, nil_deo);
	/* unused   */ uxn_port(&box->core, 0xc, nil_dei, nil_deo);
	/* unused   */ uxn_port(&box->core, 0xd, nil_dei, nil_deo);
	/* unused   */ uxn_port(&box->core, 0xe, nil_dei, nil_deo);
	/* unused   */ uxn_port(&box->core, 0xf, nil_dei, nil_deo);

	/* this is weird. i think we just want to fold it into the window d init stuff */
	return box;
}

void
LoadROMIntoBox(UxnBox *box, LPCSTR filename)
{
	DWORD bytes_read;
	if (!LoadFileInto(filename, (char *)(box + 1) + UXN_ROM_OFFSET, UXN_RAM_SIZE - UXN_ROM_OFFSET, &bytes_read)) {
		TCHAR tmp[MAX_PATH]; DWORD res = GetFullPathNameA(filename, MAX_PATH, tmp, NULL);
		if (res == 0 || res >= MAX_PATH) tmp[0] = 0;
		FatalBox("Tried and failed to load the rom file %s", tmp);
	}
}

void
FreeUxnBox(UxnBox *box)
{
	HeapFree(GetProcessHeap(), 0, box);
}

// typedef struct EmuCreationParams {
// 	// could make this a UxnBox in the future to avoid window-create-then-resize if the rom has a particular size it wants. or something like that
// 	LPSTR *rom_file;
// } EmuCreationParams;

static void SetUpBitmapInfo(BITMAPINFO *bmi, int width, int height)
{
	ZeroMemory(bmi, sizeof(BITMAPINFO));
	bmi->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
	bmi->bmiHeader.biWidth = width;
	bmi->bmiHeader.biHeight = -height;
	bmi->bmiHeader.biPlanes = 1;
	bmi->bmiHeader.biBitCount = 32;
	bmi->bmiHeader.biCompression = BI_RGB;
}

/* Will not be clipped to the client rect. May exceed it in any dimension. Use IntersectRect if you need it clipped. */
static void GetUxnScreenRect(RECT *in_clientrect, UxnScreen *screen, RECT *out_rect)
{
	LONG s_width = screen->width, s_height = screen->height;
	LONG s_x = (in_clientrect->right - s_width) / 2, s_y = (in_clientrect->bottom - s_height) / 2;
	out_rect->left = s_x; out_rect->top = s_y;
	out_rect->right = s_x + s_width; out_rect->bottom = s_y + s_height;
}
/* If in_rect is 0 size or smaller in a dimension, the point will rest against the left or top, so that bounding a point to a 0,0,0,0 rectangle puts the point at 0,0 */
static void BoundPointInRect(POINT *point, RECT *rect)
{
	if (point->x >= rect->right) point->x = rect->right - 1;
	if (point->x < rect->left) point->x = rect->left;
	if (point->y >= rect->bottom) point->y = rect->bottom - 1;
	if (point->y < rect->top) point->y = rect->top;
}

static void BindPointToLocalUxnScreen(RECT *in_screenrect, POINT *in_out_mousepoint)
{
	BoundPointInRect(in_out_mousepoint, in_screenrect);
	in_out_mousepoint->x -= in_screenrect->left; in_out_mousepoint->y -= in_screenrect->top;
}

static void SetHostCursorVisible(EmuWindow *d, BOOL visible)
{
	if (d->host_cursor == visible) return;
	d->host_cursor = visible;
	ShowCursor(visible);
}

static LRESULT CALLBACK
WindowProc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
	EmuWindow *d = (EmuWindow *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
	RECT crect, srect;
	switch (msg)
	{
		case WM_CREATE:
		{
			BITMAPINFO bmi; LPCSTR filename;
			SetUpBitmapInfo(&bmi, UXN_DEFAULT_WIDTH, UXN_DEFAULT_HEIGHT);

			d = AllocZeroedOrFail(sizeof(EmuWindow));
			SetWindowLongPtr(hwnd, GWLP_USERDATA, (LONG_PTR)d);

#if 0 /* TODO test if there's any penalty for allocating this stuff in WM_PAINT */
			HDC hDC = GetDC(hwnd);
			d->hDibDC = CreateCompatibleDC(hDC);
			d->hBMP = CreateDIBSection(d->hDibDC, &bmi, DIB_RGB_COLORS, NULL, NULL, 0);
			ReleaseDC(hwnd, hDC);
#endif
			d->host_cursor = TRUE;

			// Context *context = ((CREATESTRUCT *)lparam)->lpCreateParams;
			filename = ((CREATESTRUCT *)lparam)->lpCreateParams;

			d->box = MakeUxnBox();
			d->box->user = d;
			SetUxnScreenSize(&d->screen, UXN_DEFAULT_WIDTH, UXN_DEFAULT_HEIGHT);
			d->filer.hFile = d->filer.hFind = INVALID_HANDLE_VALUE;
			LoadROMIntoBox(d->box, filename);
			if (!uxn_eval(&d->box->core, UXN_ROM_OFFSET))
				DebugBox("Uxn boot error");

			return 0;
		}
		case WM_DESTROY:
		{
			FreeUxnBox(d->box);
			FreeUxnScreen(&d->screen);
			ResetFiler(&d->filer);
			if (d->hBMP) DeleteObject(d->hBMP);
			if (d->hDibDC) DeleteDC(d->hDibDC);
			HeapFree(GetProcessHeap(), 0, d);
			PostQuitMessage(0);
			return 0;
		}
		case WM_PAINT:
		{
			PAINTSTRUCT ps;
			HDC hDC; BITMAPINFO bmi; SIZE bmpSize;
			uxn_eval(&d->box->core, GETVECTOR(d->box->dev_screen));
			GetClientRect(hwnd, &crect);
			GetUxnScreenRect(&crect, &d->screen, &srect);
			SetUpBitmapInfo(&bmi, d->screen.width, d->screen.height);
			if (d->dib_dims.cx != d->screen.width || d->dib_dims.cy != d->screen.height)
			{
				if (d->hBMP) DeleteObject(d->hBMP);
				if (d->hDibDC) DeleteDC(d->hDibDC);
				if (d->screen.width > 0 && d->screen.height > 0)
				{
					hDC = GetDC(hwnd);
					d->hDibDC = CreateCompatibleDC(hDC);
					d->hBMP = CreateDIBSection(d->hDibDC, &bmi, DIB_RGB_COLORS, NULL, NULL, 0);
					ReleaseDC(hwnd, hDC);
				}
				else
				{
					d->hBMP = NULL;
					d->hDibDC = NULL;
				}
				d->dib_dims.cx = d->screen.width;
				d->dib_dims.cy = d->screen.height;
				d->needs_clear = 1;
			}
			if (!d->hBMP || !d->hDibDC) return 0; /* TODO should at least clear window */
			{
				DIBSECTION sec; UxnScreen *p = &d->screen;
				// SIZE_T width = MIN(p->width, sec.dsBm.bmWidth), height = MIN(p->height, sec.dsBm.bmHeight);
				// TODO deal with bitmap possibly having padding
				int i, size = p->width * p->height; ULONG palette[16];
				GetObject(d->hBMP, sizeof sec, &sec);
				for (i = 0; i < 16; i++)
					palette[i] = p->palette[(i >> 2) ? (i >> 2) : (i & 3)];
				for (i = 0; i < size; i++)
					((ULONG *)sec.dsBm.bmBits)[i] = palette[p->fg[i] << 2 | p->bg[i]];
			}
			hDC = BeginPaint(hwnd, &ps);
			if (d->needs_clear)
			{
				SelectObject(hDC, GetStockObject(BLACK_BRUSH));
				Rectangle(hDC, 0, 0, crect.right, crect.bottom);
				d->needs_clear = 0;
			}
			// One-line version, doesn't need the retained stuff, but probably slower
			// SetDIBitsToDevice(hDC, 0, 0, UXN_DEFAULT_WIDTH, UXN_DEFAULT_HEIGHT, 0, 0, 0, UXN_DEFAULT_HEIGHT, uxn_screen.pixels, &bmi, DIB_RGB_COLORS);
			// SetDIBits(d->hDibDC, d->hBMP, 0, UXN_DEFAULT_HEIGHT, d->screen.pixels, &bmi, DIB_RGB_COLORS);
			SelectObject(d->hDibDC, d->hBMP);
			// StretchBlt(hDC, 0, 0, UXN_DEFAULT_WIDTH, UXN_DEFAULT_HEIGHT, d->hDibDC, 0, 0, UXN_DEFAULT_WIDTH, UXN_DEFAULT_HEIGHT, SRCCOPY);
			BitBlt(hDC, srect.left, srect.top, d->screen.width, d->screen.height, d->hDibDC, 0, 0, SRCCOPY);
			EndPaint(hwnd, &ps);
			return 0;
		}
		case WM_SIZE:
		{
			d->needs_clear = 1;
			return 0;
		}

		{
		unsigned int or_and, bits;
		case WM_MOUSEMOVE:   or_and = 0; bits = 0x0; goto act_mouse;
		case WM_LBUTTONDOWN: or_and = 0; bits = 0x1; goto act_mouse;
		case WM_LBUTTONUP:   or_and = 1; bits = 0x1; goto act_mouse;
		case WM_MBUTTONDOWN: or_and = 0; bits = 0x2; goto act_mouse;
		case WM_MBUTTONUP:   or_and = 1; bits = 0x2; goto act_mouse;
		case WM_RBUTTONDOWN: or_and = 0; bits = 0x4; goto act_mouse;
		case WM_RBUTTONUP:   or_and = 1; bits = 0x4; goto act_mouse;
		act_mouse:
		{
			POINT mouse;
			mouse.x = LOWORD(lparam); mouse.y = HIWORD(lparam);
			GetClientRect(hwnd, &crect);
			GetUxnScreenRect(&crect, &d->screen, &srect);
			SetHostCursorVisible(d, !PtInRect(&srect, mouse));
			BindPointToLocalUxnScreen(&srect, &mouse);
			DEVPOKE16(d->box->dev_mouse, 0x2, (Uint16)mouse.x)
			DEVPOKE16(d->box->dev_mouse, 0x4, (Uint16)mouse.y)
			if (or_and) d->box->dev_mouse->dat[6] &= ~bits;
			else d->box->dev_mouse->dat[6] |= bits;
			uxn_eval(&d->box->core, GETVECTOR(d->box->dev_mouse));
			InvalidateRect(hwnd, &srect, FALSE);
			break;
		}
		case WM_MOUSEWHEEL:
		{
			short zDelta = (short)HIWORD(wparam);
			GetClientRect(hwnd, &crect);
			GetUxnScreenRect(&crect, &d->screen, &srect);
			/* could set mouse x,y pos here if we wanted to */
			DEVPOKE16(d->box->dev_mouse, 0xa, 0); /* no X axis scrolling yet */
			DEVPOKE16(d->box->dev_mouse, 0xc, (Uint16)(-zDelta / 120)); /* TODO accumulate error */
			uxn_eval(&d->box->core, GETVECTOR(d->box->dev_mouse));
			InvalidateRect(hwnd, &srect, FALSE);
			DEVPOKE16(d->box->dev_mouse, 0xa, 0);
			DEVPOKE16(d->box->dev_mouse, 0xc, 0);
			break;
		}
		}

		{ /* Keyboard input */
		TCHAR keyChar;
		case WM_CHAR:
		{
			keyChar = (TCHAR)wparam;
		as_wm_char:
			/* Disallow control characters except tab, newline, etc. */
			if (keyChar < 32 && keyChar != 8 && keyChar != 9 && keyChar != 10 && keyChar != 13 && keyChar != 27) break;
			GetClientRect(hwnd, &crect);
			GetUxnScreenRect(&crect, &d->screen, &srect);
			d->box->dev_ctrl->dat[3] = (Uint8)keyChar;
			uxn_eval(&d->box->core, GETVECTOR(d->box->dev_ctrl));
			d->box->dev_ctrl->dat[3] = 0;
			InvalidateRect(hwnd, &srect, FALSE);
			return 0;
		}
		case WM_KEYDOWN:
		{
			int old_mask = d->box->dev_ctrl->dat[2];
			if (old_mask & (0x01 | 0x02) && (keyChar = MapVirtualKey(wparam, MAPVK_VK_TO_CHAR)))
			{
				/* Make lower case if upper case */
				if (!(old_mask & 0x04) && keyChar >= 'A' && keyChar <= 'Z') keyChar += 0x20;
				goto as_wm_char;
			}
		}
		case WM_KEYUP:
		{
			int vKey = (int)wparam, mask = 0;
			switch (vKey)
			{
			case VK_CONTROL: mask = 0x01; break;
			case VK_MENU:    mask = 0x02; break; /* alt key */
			case VK_SHIFT:   mask = 0x04; break;
			case VK_HOME:    mask = 0x08; break;
			case VK_UP:      mask = 0x10; break;
			case VK_DOWN:    mask = 0x20; break;
			case VK_LEFT:    mask = 0x40; break;
			case VK_RIGHT:   mask = 0x80; break;
			}
			if (!mask) break;
			if (msg == WM_KEYDOWN) d->box->dev_ctrl->dat[2] |= mask;
			else d->box->dev_ctrl->dat[2] &= ~mask;
			GetClientRect(hwnd, &crect);
			GetUxnScreenRect(&crect, &d->screen, &srect);
			uxn_eval(&d->box->core, GETVECTOR(d->box->dev_ctrl));
			InvalidateRect(hwnd, &srect, FALSE);
			return 0;
		}
		}

		case WM_NCMOUSEMOVE:
		case WM_MOUSELEAVE:
			SetHostCursorVisible(d, TRUE);
			break;
	}
	return DefWindowProc(hwnd, msg, wparam, lparam);
}

static LPCSTR EmuWinClass = TEXT("uxn_emu_win");

HWND
CreateUxnWindow(HINSTANCE hInst, LPCSTR file)
{
	RECT rect;
	rect.left = 0; rect.top = 0;
	rect.right = rect.left + UXN_DEFAULT_WIDTH;
	rect.bottom = rect.top + UXN_DEFAULT_HEIGHT;
	AdjustWindowRect(&rect, WS_OVERLAPPEDWINDOW, FALSE);
	return CreateWindowEx(WS_EX_APPWINDOW, EmuWinClass, TEXT("Uxn"), WS_OVERLAPPEDWINDOW, 200, 200, rect.right - rect.left, rect.bottom - rect.top, NULL, NULL, hInst, (void *)file);
}

int CALLBACK
WinMain(HINSTANCE instance, HINSTANCE prev_instance, LPSTR command_line, int show_code)
{
	WNDCLASSEX wc; HWND hWin;
	MSG msg;
	(void)command_line; (void)prev_instance;
	ZeroMemory(&wc, sizeof wc);
	wc.hInstance = instance;
	wc.cbSize = sizeof wc;
	wc.lpfnWndProc = WindowProc;
	wc.lpszClassName = EmuWinClass;
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hIcon = LoadIcon(instance, (LPCTSTR)IDI_UXN32); // use this one
	// wc.hIconSm = LoadIcon(instance, (LPCTSTR)IDI_UXN32);
	wc.style = CS_HREDRAW | CS_VREDRAW;
	// wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
	RegisterClassEx(&wc);

	hWin = CreateUxnWindow(instance, "boot.rom");
	ShowWindow(hWin, show_code);

	while (GetMessage(&msg, NULL, 0, 0))
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	return 0;
}
