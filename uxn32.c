#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include "resource.h"
#include "core32.h"
#include <windows.h>
#include <shellapi.h>
#include <shlwapi.h>
#include <commdlg.h>
#include <mmsystem.h>

#pragma comment(lib, "user32.lib")
#pragma comment(lib, "gdi32.lib")
#pragma comment(lib, "shell32.lib")
#pragma comment(lib, "shlwapi.lib")
#pragma comment(lib, "comdlg32.lib")
#pragma comment(lib, "winmm.lib")

#if !defined(_WIN64) && _WINVER < 0x0500
#define GetWindowLongPtrA   GetWindowLongA
#define GetWindowLongPtrW   GetWindowLongW
#define GetWindowLongPtr    GetWindowLong
#define SetWindowLongPtrA   SetWindowLongA
#define SetWindowLongPtrW   SetWindowLongW
#define SetWindowLongPtr    SetWindowLong
#define GWLP_WNDPROC    GWL_WNDPROC
#define GWLP_HINSTANCE  GWL_HINSTANCE
#define GWLP_HWNDPARENT GWL_HWNDPARENT
#define GWLP_USERDATA   GWL_USERDATA
#define GWLP_ID         GWL_ID
#define DWLP_MSGRESULT  DWL_MSGRESULT
#define DWLP_DLGPROC    DWL_DLGPROC
#define DWLP_USER       DWL_USER
typedef LONG LONG_PTR;
typedef unsigned long ULONG_PTR, *PULONG_PTR;
typedef ULONG_PTR DWORD_PTR, *PDWORD_PTR;
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
#define MAPVK_VK_TO_CHAR (2)
#endif

#define OFFSET_OF(s, m) ((SIZE_T)&(((s*)0)->m))
#define OUTER_OF(outer, type, field) ((type *) ((char *)(outer) - OFFSET_OF(type, field)))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#ifdef NDEBUG
#define DEBUG_CHECK(a)
#else
#define DEBUG_CHECK(a) { if (!(a)) DebugBreak(); }
#endif

#define UXN_DEFAULT_WIDTH (64 * 8)
#define UXN_DEFAULT_HEIGHT (40 * 8)
#define UXN_RAM_SIZE (0x10000u)
#define UXN_ROM_OFFSET (0x0100u)

#define DEVPEEK(d, o, x) ((o) = ((d)->dat[(x)] << 8) + (d)->dat[(x) + 1])
#define DEVPOKE(d, a, v) ((d)->dat[(a)] = (v) >> 8, (d)->dat[(a) + 1] = (v))
#define DEVPEEK2(d, o1, o2, a) (DEVPEEK(d, o1, a), DEVPEEK(d, o2, a + 2))
#define DEVPOKE2(d, a, v1, v2) (DEVPOKE(d, a, v1), DEVPOKE(d, a + 2, v2))
#define GETVECTOR(d) ((d)->dat[0] << 8 | (d)->dat[1])

static LARGE_INTEGER _perfcount_freq;
static LONGLONG ExecutionTimeLimit;
#define RepaintTimeLimit ExecutionTimeLimit

static LONGLONG LongLongMulDiv(LONGLONG value, LONGLONG numer, LONGLONG denom)
{
    LONGLONG q = value / denom, r = value % denom;
    return q * numer + r * numer / denom;
}

static LONGLONG TimeStampNow(void) { LARGE_INTEGER r; QueryPerformanceCounter(&r); return r.QuadPart; }
static UINT TimeStampMicros(LONGLONG t) { return (UINT)LongLongMulDiv(t, 1000000, _perfcount_freq.QuadPart); }
static UINT TimeStampMillis(LONGLONG t) { return (UINT)LongLongMulDiv(t, 1000, _perfcount_freq.QuadPart); }
static UINT MicrosSince(LONGLONG t) { return TimeStampMicros(TimeStampNow() - t); }

typedef struct ListLink { struct ListLink *prev, *next; } ListLink;
typedef struct LinkedList { struct ListLink *front, *back; } LinkedList;

static ListLink * _impl_ListPopFront(LinkedList *list)
{
	ListLink *a = list->front;
	DEBUG_CHECK(a);
	list->front = a->next;
	if (a == list->back) list->back = NULL;
	else a->next = list->front->prev = NULL;
	return a;
}
static void _impl_ListPushBack(LinkedList *list, ListLink *node)
{
	DEBUG_CHECK(node->prev == NULL && node->next == NULL);
	if (list->back) { node->prev = list->back; list->back->next = node; }
	else { list->front = node; }
	list->back = node;
}
static void _impl_ListRemove(LinkedList *list, ListLink *a)
{
	if (list->front != a && !a->prev) return; /* Do nothing if not in list */
	DEBUG_CHECK((a == list->back)  == (a->next == NULL));
	DEBUG_CHECK((a == list->front) == (a->prev == NULL));
	if (a->prev) a->prev->next = a->next;
	else list->front = a->next;
	if (a->next) a->next->prev = a->prev;
	else list->back = a->prev;
	a->next = a->prev = NULL;
}
#define ListPopFront(list, type, link_field) OUTER_OF(_impl_ListPopFront(list), type, link_field)
#define ListPushBack(list, a, link_field) _impl_ListPushBack((list), &(a)->link_field)
#define ListRemove(list, a, link_field) _impl_ListRemove((list), &(a->link_field))

#define UXN_SAMPLE_RATE 44100
#define UXN_VOICES 4

typedef struct UxnVoice {
	Uint8 *addr;
	Uint32 count, advance, period, age, a, d, s, r;
	Uint16 i, len;
	Sint8 volume[2];
	Uint8 pitch, repeat;
} UxnVoice;

typedef struct UxnSynth {
	UxnVoice voices[UXN_VOICES];
} UxnSynth;

typedef struct UxnWaveOut {
	HWAVEOUT hWaveOut;
	WAVEHDR waveHdrs[2];
	SHORT *sampleBuffers[2];
	BYTE pick;
	BYTE ready;
} UxnWaveOut;

typedef struct UxnBox
{
	void *user;
	Uxn core;
	Stack work_stack, ret_stack;
	Uint8 device_memory[256];
} UxnBox;
typedef struct UxnScreen
{
	Uint32 palette[4];
	LONG width, height;
	Uint8 *bg, *fg;
} UxnScreen;
typedef struct UxnFiler
{
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

enum { Screen60hzTimer = 1, InitAudioTimer };
enum
{
	UXNMSG_ContinueExec = WM_USER,
	UXNMSG_BecomeClone
};
enum EmuIn
{
	EmuIn_KeyChar = 1,
	EmuIn_CtrlDown,
	EmuIn_CtrlUp,
	EmuIn_ResetKeys,
	EmuIn_MouseDown,
	EmuIn_MouseUp,
	EmuIn_Wheel,
	EmuIn_Screen,
	EmuIn_Start
};
typedef struct EmuInEvent
{
	BYTE type, bits;
	USHORT x, y;
} EmuInEvent;

#define QUEUE_CAP 256

typedef struct EmuWindow
{
	UxnBox *box;
	Device *dev_screen, *dev_mouse, *dev_ctrl, *dev_audio0;
	HWND hWnd;
	HBITMAP hBMP;
	HDC hDibDC;
	SIZE dib_dims;

	BYTE running, exec_state, needs_clear, host_cursor;

	EmuInEvent *queue_buffer;
	USHORT queue_count, queue_first;
	ListLink work_link;
	LONGLONG last_paint;

	RECT viewport_rect;
	LONG viewport_scale; /* really only need 1 bit for this... */
	UxnScreen screen;
	UxnFiler filer;
	UxnSynth synth;
	UxnWaveOut *wave_out;

	BYTE needs_audio; /* 3 states, 0: not needed, 1: plz init, 2: init started */
	TCHAR rom_path[MAX_PATH];
} EmuWindow;

static Uint8 nil_dei(Device *d, Uint8 port) { return d->dat[port]; }
static void  nil_deo(Device *d, Uint8 port) { (void)d;(void)port; }

static int VFmtBox(HWND hWnd, LPCSTR title, UINT flags, char const *fmt, va_list ap)
{
	char buffer[1024];
	int res = wvsprintfA(buffer, fmt, ap);
	if (res < 0 || res >= 1024) DebugBreak();
	buffer[res] = 0;
	return MessageBox(hWnd, buffer, title, flags);
}
static int FmtBox(HWND hWnd, LPCSTR title, UINT flags, char const *fmt, ...)
{
	int res; va_list ap; va_start(ap, fmt);
	res = VFmtBox(hWnd, title, flags, fmt, ap);
	va_end(ap);
	return res;
}
static __declspec(noreturn) void FatalBox(char const *fmt, ...)
{
	va_list ap; va_start(ap, fmt);
	while (ShowCursor(TRUE) < 0);
	VFmtBox(0, TEXT("Major Problem"), MB_OK | MB_ICONSTOP | MB_TASKMODAL, fmt, ap);
	va_end(ap);
	ExitProcess(ERROR_GEN_FAILURE);
}
static __declspec(noreturn) void OutOfMemory(void)
{
	MessageBox(NULL, TEXT("Out of memory"), NULL, MB_OK | MB_ICONSTOP | MB_TASKMODAL);
	ExitProcess(ERROR_OUTOFMEMORY);
}

#ifndef NDEBUG
static void DebugPrint(char const *fmt, ...)
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
static void PrintLastError(void)
{
	DWORD dw = GetLastError();
	LPVOID lpMsgBuf;
	FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, dw, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR) &lpMsgBuf, 0, NULL );
	DebugPrint("Win32 error %d: %s", (int)dw, lpMsgBuf);
}
static void DebugBox(char const *fmt, ...)
{
	va_list ap; va_start(ap, fmt);
	VFmtBox(0, TEXT("Debug"), MB_OK, fmt, ap);
	va_end(ap);
}
#endif

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

static BOOL LoadFileInto(LPCSTR path, char *dest, DWORD max_bytes, DWORD *bytes_read)
{
	HANDLE hFile = CreateFile(path, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hFile == INVALID_HANDLE_VALUE) return FALSE;
	if (!ReadFile(hFile, dest, max_bytes, bytes_read, NULL))
		FatalBox("Read error while reading file %s", path);
	CloseHandle(hFile);
	return TRUE;
}

static const Uint8 SpriteBlendingTable[5][16] = {
	{0, 0, 0, 0, 1, 0, 1, 1, 2, 2, 0, 2, 3, 3, 3, 0},
	{0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3},
	{1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1},
	{2, 3, 1, 2, 2, 3, 1, 2, 2, 3, 1, 2, 2, 3, 1, 2},
	{1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0}};

static void DrawUxnSprite(UxnScreen *p, Uint8 *layer_pixels, Uint16 x, Uint16 y, Uint8 *sprite, Uint8 color, Uint8 flipx, Uint8 flipy, Uint8 twobpp)
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
	/* TODO pad for simd */
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

EmuWindow * EmuOfDevice(Device *d) /* TODO these are kinda dumb, clean when making Devices better */
{
	UxnBox *box = OUTER_OF(d->u, UxnBox, core);
	return (EmuWindow *)box->user;
}

UxnScreen * ScreenOfDevice(Device *d)
{
	UxnBox *box = OUTER_OF(d->u, UxnBox, core);
	return &((EmuWindow *)box->user)->screen;
}

Uint8 ScreenDevInCb(Device *d, Uint8 port)
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

static void RefitEmuWindow(EmuWindow *d)
{
	RECT c, r;
	c.left = 0; c.top = 0;
	c.right = d->screen.width * d->viewport_scale; c.bottom = d->screen.height * d->viewport_scale;
	AdjustWindowRect(&c, GetWindowLong(d->hWnd, GWL_STYLE), TRUE); /* note: no spam protection from Uxn program yet */
	GetWindowRect(d->hWnd, &r);
	MoveWindow(d->hWnd, r.left, r.top, c.right - c.left, c.bottom - c.top, TRUE);
	/* Seems like the repaint from this is always async. We need the TRUE flag for repainting or the non-client area will be messed up on non-DWM. */
}

void ScreenDevOutCb(Device *d, Uint8 port)
{
	UxnScreen *screen = ScreenOfDevice(d);
	Uxn *u = d->u; /* TODO */
	switch(port) {
	case 0x5:
	{
		DWORD w, h;
		DEVPEEK2(d, w, h, 0x2);
		if (w > 1024 || h > 1024)
		{
			/* If the size is unacceptable, write back the old one */
			DEVPOKE2(d, 0x2, screen->width, screen->height);
		}
		else
		{
			SetUxnScreenSize(screen, w, h);
			RefitEmuWindow(EmuOfDevice(d));
		}
		break;
	}
	case 0xe:
	{
		Uint16 x, y;
		Uint8 layer = d->dat[0xe] & 0x40, *pixels = layer ? screen->fg : screen->bg;
		int width = screen->width;
		DEVPEEK2(d, x, y, 0x8);
		if (x < width && y < screen->height) /* poke pixel */
			pixels[x + y * width] = d->dat[0xe] & 0x3;
		if(d->dat[0x6] & 0x01) DEVPOKE(d, 0x8, x + 1); /* auto x+1 */
		if(d->dat[0x6] & 0x02) DEVPOKE(d, 0xa, y + 1); /* auto y+1 */
		break;
	}
	case 0xf:
	{
		UINT x, y, addr, tmp, advnc = d->dat[0x6], sprite = d->dat[0xf];
		UINT twobpp = !!(sprite & 0x80);
		Uint8 *layer_pixels = (sprite & 0x40) ? screen->fg : screen->bg;
		DEVPEEK2(d, x, y, 0x8);
		DEVPEEK(d, addr, 0xc);
		DrawUxnSprite(screen, layer_pixels, x, y, &u->ram[addr], sprite & 0xf, sprite & 0x10, sprite & 0x20, twobpp);
		/* auto addr+length */
		if(advnc & 0x04) { tmp = addr + 8 + twobpp * 8; DEVPOKE(d, 0xc, tmp); }
		if(advnc & 0x01) { tmp = x + 8; DEVPOKE(d, 0x8, tmp); } /* auto x+8 */
		if(advnc & 0x02) { tmp = y + 8; DEVPOKE(d, 0xa, tmp); } /* auto y+8 */
		break;
	}
	}
}

Uint8 SystemDevInCb(Device *d, Uint8 port)
{
	switch (port)
	{
	case 0x2: return d->u->wst->ptr;
	case 0x3: return d->u->rst->ptr;
	}
	return d->dat[port];
}

void SystemDevOutCb(Device *d, Uint8 port)
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

static void ResetFiler(UxnFiler *f)
{
	if (f->hFile != INVALID_HANDLE_VALUE) { CloseHandle(f->hFile); f->hFile = INVALID_HANDLE_VALUE; }
	if (f->hFind != INVALID_HANDLE_VALUE) { FindClose(f->hFind); f->hFind = INVALID_HANDLE_VALUE; }
	f->state = FileDevState_Init;
}

static UxnFiler *FilerOfDevice(Device *d)
{
	UxnBox *box = OUTER_OF(d->u, UxnBox, core);
	return &((EmuWindow *)box->user)->filer;
}

/* Returns 0 on error, including not enough space. Will not write to the dst buffer on error.
 * dst_len should be at least strlen(display_name) + 7 or it will definitely not work. */
/* TODO error when file size beyond DWORD */
static DWORD PrintDirListRow(char *dst, DWORD dst_len, char *display_name, DWORD file_size, DWORD is_dir)
{
	int written; int ok = file_size < 0x10000; char tmp[1024 + 1];
	if (dst_len > sizeof tmp) dst_len = sizeof tmp;
	if (is_dir || !ok) written = wsprintfA(tmp, ok ? "---- %s\n" : "???? %s\n", display_name);
	else written = wsprintfA(tmp, "%04x %s\n", (unsigned int)file_size, display_name);
	if (written <= 0 || written >= (int)dst_len - 1) return 0;
	CopyMemory(dst, tmp, (DWORD)written + 1);
	return (DWORD)written;
}

static void FileDevPathChange(Device *d)
{
	DWORD addr, i, avail;
	char tmp[MAX_PATH + 1], *in_mem;
	UxnFiler *f = FilerOfDevice(d);
	Uxn *u = d->u; /* TODO */
	ResetFiler(f);
	DEVPEEK(d, addr, 0x8);
	avail = UXN_RAM_SIZE - addr;
	in_mem = (char *)u->ram + addr;
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

static DWORD FileDevRead(UxnFiler *f, char *dst, DWORD dst_len)
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
			/* DWORD copy = WideCharToMultiByte(1252, 0, ) */
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

static DWORD FileDevWrite(UxnFiler *f, char *src, DWORD src_len, int flags)
{
	DWORD written;
	if (f->state != FileDevState_Writing) {
		int append = flags & 0x01;
		ResetFiler(f);
		f->hFile = CreateFile(f->path, GENERIC_WRITE, FILE_SHARE_READ, NULL, append ? OPEN_ALWAYS : CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	}
	if (f->hFile == INVALID_HANDLE_VALUE) return 0;
	if (!WriteFile(f->hFile, src, src_len, &written, NULL)) {
		ResetFiler(f); /* TODO signal error to user */
		return 0;
	}
	return written;
}

static DWORD FileDevStat(UxnFiler *f, char *dst, DWORD dst_len)
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

static DWORD FileDevDelete(UxnFiler *f)
{
	DWORD result;
	ResetFiler(f);
	if (!f->pathlen) return 0;
	if (PathIsDirectoryA(f->path))
		result = RemoveDirectoryA(f->path);
	else
		result = DeleteFileA(f->path);
	return result ? 0 : 1;
}

#define NOTE_PERIOD (UXN_SAMPLE_RATE * 0x4000 / 11025)
#define ADSR_STEP (UXN_SAMPLE_RATE / 0xf)

static const Uint32 advances[12] = {
	0x80000, 0x879c8, 0x8facd, 0x9837f, 0xa1451, 0xaadc1,
	0xb504f, 0xbfc88, 0xcb2ff, 0xd7450, 0xe411f, 0xf1a1c
};

static INT VoiceEnvelope(UxnVoice *c, Uint32 age)
{
	if(!c->r) return 0x0888;
	if(age < c->a) return 0x0888 * age / c->a;
	if(age < c->d) return 0x0444 * (2 * c->d - c->a - age) / (c->d - c->a);
	if(age < c->s) return 0x0444;
	if(age < c->r) return 0x0444 * (c->r - age) / (c->r - c->s);
	c->advance = 0;
	return 0x0000;
}

int VoiceRender(UxnVoice *c, SHORT *out, SHORT *end)
{
	INT s;
	if (!c->advance || !c->period) return 0;
	while (out < end)
	{
		c->count += c->advance;
		c->i += c->count / c->period; /* TODO div and also remainder? yikes */
		c->count %= c->period;
		if (c->i >= c->len)
		{
			if (!c->repeat) { c->advance = 0; break; }
			c->i %= c->len;
		}
		s = (Sint8)(c->addr[c->i] + 0x80) * VoiceEnvelope(c, c->age++);
		*out++ += s * c->volume[0] / (0x180 * 2); /* Original: / (0x180 * 1) */
		*out++ += s * c->volume[1] / (0x180 * 2); /* Temporarily make this quieter until we add volume slider */
	}
	/* if(!c->advance) audio_is_finished(c); */
	return 1;
}

void VoiceStart(UxnVoice *c, Uint16 adsr, Uint8 pitch)
{
	if (pitch < 108 && c->len)
		c->advance = advances[pitch % 12] >> (8 - pitch / 12);
	else
	{
		c->advance = 0;
		return;
	}
	c->a = ADSR_STEP * (adsr >> 12);
	c->d = ADSR_STEP * (adsr >> 8 & 0xf) + c->a;
	c->s = ADSR_STEP * (adsr >> 4 & 0xf) + c->d;
	c->r = ADSR_STEP * (adsr >> 0 & 0xf) + c->s;
	c->age = 0;
	c->i = 0;
	if(c->len <= 0x100) /* single cycle mode */
		c->period = NOTE_PERIOD * 337 / 2 / c->len;
	else /* sample repeat mode */
		c->period = NOTE_PERIOD;
}

Uint8 VoiceCalcVU(UxnVoice *c)
{
	int i;
	INT sum[2] = {0, 0};
	if (!c->advance || !c->period) return 0;
	for (i = 0; i < 2; i++)
	{
		if (!c->volume[i]) continue;
		sum[i] = 1 + VoiceEnvelope(c, c->age) * c->volume[i] / 0x800;
		if (sum[i] > 0xf) sum[i] = 0xf;
	}
	return (sum[0] << 4) | sum[1];
}

#define AUDIO_BUF_SAMPLES 2048

static void WriteOutSynths(EmuWindow *d)
{
	WAVEHDR *hdr = &d->wave_out->waveHdrs[d->wave_out->pick];
	SHORT *samples = d->wave_out->sampleBuffers[d->wave_out->pick];
	MMRESULT res; int i, still_running;
	if (!d->wave_out->hWaveOut) return;
	d->wave_out->pick = 1 - d->wave_out->pick;
	ZeroMemory(hdr, sizeof(WAVEHDR));
	hdr->dwBufferLength = AUDIO_BUF_SAMPLES * 2 * sizeof(SHORT);
	hdr->lpData = (LPSTR)samples;
	ZeroMemory(samples, AUDIO_BUF_SAMPLES * 2 * sizeof(SHORT));
	for (i = still_running = 0; i < UXN_VOICES; i++)
	{
		still_running |= VoiceRender(&d->synth.voices[i], samples, samples + AUDIO_BUF_SAMPLES * 2);
	}
	res = waveOutPrepareHeader(d->wave_out->hWaveOut, hdr, sizeof(WAVEHDR));
	res = waveOutWrite(d->wave_out->hWaveOut, hdr, sizeof(WAVEHDR));
}

static void CALLBACK WaveOutCallback(HWAVEOUT hwo, UINT uMsg, DWORD_PTR dwInstance, DWORD_PTR dwParam1, DWORD_PTR dwParam2)
{
	EmuWindow *d = (EmuWindow *)dwInstance;
	switch (uMsg)
	{
	case WOM_OPEN: break;
	case WOM_CLOSE: d->wave_out->hWaveOut = NULL; break;
	case WOM_DONE:
		waveOutUnprepareHeader(hwo, (WAVEHDR *)dwParam1, sizeof(WAVEHDR));
		WriteOutSynths(d);
		break;
	}
	(void)dwParam2;
}

static void InitWaveOutAudio(EmuWindow *d)
{
	MMRESULT mmRes;
	WAVEFORMATEX pcm; /* PCMWAVEFORMAT will not work on 64-bit due to padding, MS docs are fucked */
	DEBUG_CHECK(d->wave_out == 0);
	d->wave_out = AllocZeroedOrFail(sizeof(UxnWaveOut));
	d->wave_out->sampleBuffers[0] = AllocZeroedOrFail(AUDIO_BUF_SAMPLES * 2 * sizeof(SHORT) * 2);
	d->wave_out->sampleBuffers[1] = d->wave_out->sampleBuffers[0] + AUDIO_BUF_SAMPLES * 2;
	ZeroMemory(&pcm, sizeof pcm);
	pcm.wFormatTag = WAVE_FORMAT_PCM;
	pcm.nChannels = 2;
	pcm.nSamplesPerSec = 44100;
	pcm.nAvgBytesPerSec = 44100 * 4;
	pcm.nBlockAlign = 4;
	pcm.wBitsPerSample = 16;
	mmRes = waveOutOpen(&d->wave_out->hWaveOut, WAVE_MAPPER, (LPCWAVEFORMATEX)&pcm, (DWORD_PTR)WaveOutCallback, (DWORD_PTR)d, CALLBACK_FUNCTION);
	if (mmRes == 0)
	{
		/* Can't use this yet, as it will overwrite user control of the Windows mixer */
		/* waveOutSetVolume(d->wave_out->hWaveOut, MAKELONG(0xAAAA, 0xAAAA)); */
		WriteOutSynths(d);
		WriteOutSynths(d);
	}
}

static Uint8 AudioDevInCb(Device *dev, Uint8 port)
{
	UxnBox *box = OUTER_OF(dev->u, UxnBox, core); /* TODO you know this mess */
	EmuWindow *win = (EmuWindow *)box->user;
	UxnVoice *voice = &win->synth.voices[dev - win->dev_audio0];
	if (!win->wave_out || !win->wave_out->hWaveOut) return dev->dat[port];
	switch (port)
	{
	case 0x4: return VoiceCalcVU(voice);
	case 0x2: DEVPOKE(dev, 0x2, voice->i); break;
	}
	return dev->dat[port];
}

void AudioDevOutCb(Device *dev, Uint8 port)
{
	UxnBox *box = OUTER_OF(dev->u, UxnBox, core); /* TODO you know this mess */
	EmuWindow *win = (EmuWindow *)box->user;
	UxnVoice *voice = &win->synth.voices[dev - win->dev_audio0];
	Uint16 addr, adsr;
	if (port != 0xF) return;
	DEVPEEK(dev, adsr, 0x8);
	DEVPEEK(dev, voice->len, 0xa);
	DEVPEEK(dev, addr, 0xc);
	voice->addr = &box->core.ram[addr]; /* TODO out of bounds possible? */
	voice->volume[0] = dev->dat[0xe] >> 4;
	voice->volume[1] = dev->dat[0xe] & 0xf;
	voice->repeat = !(dev->dat[0xf] & 0x80);
	VoiceStart(voice, adsr, dev->dat[0xf] & 0x7f);
	/* Defer initializing audio until after at least one paint event, because the window might not be visible yet, and this can cause a 50ms+ freeze, increasing the delay before the window is shown. */
	if (!win->needs_audio) win->needs_audio = 1;
}

Device *uxn_port(Uxn *u, Uint8 *devpage, Uint8 id, Uint8 (*deifn)(Device *d, Uint8 port), void (*deofn)(Device *d, Uint8 port))
{
	Device *d = &u->dev[id];
	d->u = u;
	d->dei = deifn;
	d->deo = deofn;
	d->dat = devpage + id * 0x10;
	return d;
}

void FileDevOutCb(Device *d, Uint8 port)
{
	DWORD result = 0, /* next inits suppress msvc warning */ out_len = 0; char *out = 0;
	UxnFiler *f = FilerOfDevice(d);
	Uxn *u = d->u; /* TODO */
	switch(port) { /* These need write location and size */
	int peek_at; DWORD dst, avail;
	case 0x5: peek_at = 0x4; goto calc;
	case 0xd: peek_at = 0xc; goto calc;
	case 0xf: peek_at = 0xe; goto calc;
	calc:
		DEVPEEK(d, dst, peek_at);
		DEVPEEK(d, out_len, 0xa);
		avail = UXN_RAM_SIZE - dst;
		if (out_len > avail) out_len = avail;
		out = (char *)u->ram + dst;
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
	DEVPOKE(d, 0x2, result);
}

#define console_deo nil_deo
#define audio_dei nil_dei
#define audio_deo nil_deo

BOOL LoadROMIntoBox(UxnBox *box, LPCSTR filename)
{
	DWORD bytes_read;
	BOOL result = LoadFileInto(filename, (char *)(box + 1) + UXN_ROM_OFFSET, UXN_RAM_SIZE - UXN_ROM_OFFSET, &bytes_read);
	if (!result)
	{
		TCHAR tmp[MAX_PATH]; DWORD res = GetFullPathNameA(filename, MAX_PATH, tmp, NULL);
		if (res == 0 || res >= MAX_PATH) tmp[0] = 0;
		FmtBox(0, "ROM File Load Error", MB_OK | MB_ICONWARNING, "Tried and failed to load the ROM file:\n\n%s\n\nDoes it exist?", tmp);
	}
	return result;
}

void InitEmuWindow(EmuWindow *d, HWND hWnd)
{
	char *main_ram;
	UxnBox *box = (UxnBox *)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(UxnBox) + UXN_RAM_SIZE);
	if (!box) OutOfMemory();
	main_ram = (char *)(box + 1);
	box->user = d;
	box->core.ram = (Uint8 *)main_ram;
	box->core.wst = &box->work_stack;
	box->core.rst = &box->ret_stack;
	/* system   */ uxn_port(&box->core, box->device_memory, 0x0, SystemDevInCb, SystemDevOutCb);
	/* console  */ uxn_port(&box->core, box->device_memory, 0x1, nil_dei, console_deo); /* ask if this should be shown in a console window on win32 and whether or not uxn roms tend to just passively poop out stuff in the background */
	/* screen   */ d->dev_screen = uxn_port(&box->core, box->device_memory,  0x2, ScreenDevInCb, ScreenDevOutCb);
	/* audio0   */ d->dev_audio0 = uxn_port(&box->core, box->device_memory,  0x3, AudioDevInCb, AudioDevOutCb);
	/* audio1   */ uxn_port(&box->core, box->device_memory,  0x4, AudioDevInCb, AudioDevOutCb);
	/* audio2   */ uxn_port(&box->core, box->device_memory,  0x5, AudioDevInCb, AudioDevOutCb);
	/* audio3   */ uxn_port(&box->core, box->device_memory,  0x6, AudioDevInCb, AudioDevOutCb);
	/* unused   */ uxn_port(&box->core, box->device_memory,  0x7, nil_dei, nil_deo);
	/* control  */ d->dev_ctrl = uxn_port(&box->core, box->device_memory,  0x8, nil_dei, nil_deo);
	/* mouse    */ d->dev_mouse = uxn_port(&box->core, box->device_memory,  0x9, nil_dei, nil_deo);
	/* file     */ uxn_port(&box->core, box->device_memory,  0xa, nil_dei, FileDevOutCb);
	/* datetime */ uxn_port(&box->core, box->device_memory,  0xb, SystemDevDateInCb, nil_deo);
	/* unused   */ uxn_port(&box->core, box->device_memory,  0xc, nil_dei, nil_deo);
	/* unused   */ uxn_port(&box->core, box->device_memory,  0xd, nil_dei, nil_deo);
	/* unused   */ uxn_port(&box->core, box->device_memory,  0xe, nil_dei, nil_deo);
	/* unused   */ uxn_port(&box->core, box->device_memory,  0xf, nil_dei, nil_deo);

	d->box = box;
	d->host_cursor = TRUE;
	d->hWnd = hWnd; /* TODO cleanup reorder these assignments */
	d->viewport_scale = 1;
	SetUxnScreenSize(&d->screen, UXN_DEFAULT_WIDTH, UXN_DEFAULT_HEIGHT);
#if 0 /* TODO test if there's any penalty for allocating this stuff in WM_PAINT */
	HDC hDC = GetDC(hwnd);
	d->hDibDC = CreateCompatibleDC(hDC);
	d->hBMP = CreateDIBSection(d->hDibDC, &bmi, DIB_RGB_COLORS, NULL, NULL, 0);
	ReleaseDC(hwnd, hDC);
#endif
	d->filer.hFile = d->filer.hFind = INVALID_HANDLE_VALUE;
}

void FreeUxnBox(UxnBox *box)
{
	HeapFree(GetProcessHeap(), 0, box);
}

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

static void CalcUxnViewport(EmuWindow *d)
{
	RECT crect, *vprect = &d->viewport_rect; LONG s_width, s_height;
	GetClientRect(d->hWnd, &crect);
	s_width = d->screen.width * 2, s_height = d->screen.height * 2;
	if (s_width <= crect.right && s_height <= crect.bottom) d->viewport_scale = 2;
	else
	{
		d->viewport_scale = 1;
		s_width = d->screen.width, s_height = d->screen.height;
	}
	vprect->left = (crect.right - s_width) / 2, vprect->top = (crect.bottom - s_height) / 2;
	vprect->right = vprect->left + s_width; vprect->bottom = vprect->top + s_height;
}

/* If in_rect is 0 size or smaller in a dimension, the point will rest against the left or top, so that bounding a point to a 0,0,0,0 rectangle puts the point at 0,0 */
static void BoundPointInRect(POINT *point, RECT *rect)
{
	if (point->x >= rect->right) point->x = rect->right - 1;
	if (point->x < rect->left) point->x = rect->left;
	if (point->y >= rect->bottom) point->y = rect->bottom - 1;
	if (point->y < rect->top) point->y = rect->top;
}

static void BindPointToLocalUxnScreen(RECT *in_screenrect, LONG scale, POINT *in_out_mousepoint)
{
	BoundPointInRect(in_out_mousepoint, in_screenrect);
	in_out_mousepoint->x -= in_screenrect->left; in_out_mousepoint->y -= in_screenrect->top;
	if (scale == 2) { in_out_mousepoint->x /= 2;in_out_mousepoint->y /= 2; }
}

static void SetHostCursorVisible(EmuWindow *d, BOOL visible)
{
	if (d->host_cursor == visible) return;
	d->host_cursor = visible;
	/* Correctly handle moving mouse between 2 overlapped emu windows. Causes WM_MOUSELEAVE to be generated.
	 * If we don't do this, cursor might not appear in non-client area.
	 * Requires NT4 or above. Could replicate with timer and checking mouse pos.
	 * Alternative: when an emu window takes the mouse, broadcast an event to the others? */
	if (!visible)
	{
		TRACKMOUSEEVENT track;
		track.cbSize = sizeof track;
		track.dwFlags = TME_LEAVE;
		track.hwndTrack = d->hWnd;
		track.dwHoverTime = 0;
		TrackMouseEvent(&track);
	}
	ShowCursor(visible);
}

static void InvalidateUxnScreenRect(EmuWindow *d)
{
	if (d->viewport_rect.right && d->viewport_rect.bottom)
		InvalidateRect(d->hWnd, &d->viewport_rect, FALSE);
}

static BOOL IllegalInstrunctionDialog(EmuWindow *d)
{
	int res; BOOL retry; unsigned int fcode = d->box->core.fault_code;
	LPCSTR place = fcode & 1 ? "Return" : "Working";
	LPCSTR action = fcode < 2 ? "underflow" : fcode < 4 ? "overflow" : "division by zero";
	ShowCursor(TRUE);
	res = FmtBox(d->hWnd, TEXT("Uxn Program Fault"), MB_RETRYCANCEL | MB_ICONEXCLAMATION, TEXT("Fault: %s-stack %s\n\nInstruction: %04x\t%Address: 0x%04x\n\nThe Uxn program performed an instruction which caused a virtual hardware fault.\n\nThis probably means there was an error in the Uxn program.\n\nYou can either retry execution while skipping over the bad instruction and hope that it works, or cancel execution and end the program."), place, action, d->box->core.ram[d->box->core.pc], d->box->core.pc);
	ShowCursor(FALSE);
	retry = res == IDRETRY;
	if (retry) { d->box->core.pc++; d->box->core.fault_code = 0; }
	/* TODO ^- skipping over div by 0 should push 0 onto stack, it's going to be unbalanced like this */
	return retry;
}

static LinkedList emus_needing_work;
static int emu_window_count;

static void ResetVM(EmuWindow *d)
{
	d->box->core.fault_code = 0;
	d->exec_state = 0;
	ZeroMemory(&d->box->work_stack, sizeof(Stack) * 2); /* optional for quick reload */
	ZeroMemory(d->box->device_memory, sizeof d->box->device_memory); /* optional for quick reload */
	DEVPOKE2(d->dev_screen, 0x2, d->screen.width, d->screen.height); /* Restore this in case ROM reads it */
	ZeroMemory((char *)(d->box + 1), UXN_RAM_SIZE);
	ZeroMemory(d->screen.palette, sizeof d->screen.palette); /* optional for quick reload */
	ZeroMemory(d->screen.bg, d->screen.width * d->screen.height * 2);
	ResetFiler(&d->filer);
}

static void SynthesizeMouseMoveToCurrent(EmuWindow *d)
{
	/* Tnstead of factoring out the event code to a function and having the WindowProc call it,this is a temp hacky solution, because I still haven't made up my mind about how it should look. */
	POINT mouse; RECT crect;
	if (GetCursorPos(&mouse) && ScreenToClient(d->hWnd, &mouse) && GetClientRect(d->hWnd, &crect) && PtInRect(&crect, mouse))
		PostMessage(d->hWnd, WM_MOUSEMOVE, 0, MAKELPARAM(mouse.x, mouse.y));
	else
		SetHostCursorVisible(d, TRUE);
}

static void PauseVM(EmuWindow *d)
{
	if (!d->running) return;
	d->queue_count = 0;
	d->running = 0;
	KillTimer(d->hWnd, Screen60hzTimer);
	SetHostCursorVisible(d, TRUE);
	ListRemove(&emus_needing_work, d, work_link);
}

static void UnpauseVM(EmuWindow *d)
{
	if (d->running) return;
	d->queue_count = 0;
	d->running = 1;
	SetTimer(d->hWnd, Screen60hzTimer, 16, NULL);
	if (d->exec_state) ListPushBack(&emus_needing_work, d, work_link);
	SynthesizeMouseMoveToCurrent(d); /* Runs async for now */
	/* Syncing held keys isn't so easy... */
}

/* TODO there's something fancy we should do with the loop to make it tell if it ran out or not by return value, returning 0 when limit is 0 means we might have succeeded in reaching the null instruction on the last allowed step, so we need to do something else */
static void RunUxn(EmuWindow *d)
{
	UINT res; Uxn *u = &d->box->core;
	LONGLONG total = 0, t_a, t_b, t_delta;
	int instr_interrupts = 0;
	if (!u->pc) goto completed;
	t_a = TimeStampNow();
	for (;;)
	{
		res = UxnExec(&d->box->core, 100000); /* about 1900 usecs on good hardware */
		instr_interrupts++;
		t_b = TimeStampNow();
		t_delta = t_b - t_a;
		/* if (u->fault_code && !IllegalInstrunctionDialog(d)) goto died; */
		if (u->fault_code) goto died;
		if (res != 0) { total += t_delta; goto completed; }
		if (t_delta > ExecutionTimeLimit) { total += t_delta; goto residual; }
		/* total will include some non-Uxn work, but close enough */
	}
died:
	PauseVM(d);
	InvalidateUxnScreenRect(d);
	if (IllegalInstrunctionDialog(d)) UnpauseVM(d);
	return;
completed:
	switch ((enum EmuIn)d->exec_state)
	{
	case EmuIn_CtrlDown:
	case EmuIn_CtrlUp:
	case EmuIn_ResetKeys:
	case EmuIn_MouseDown:
	case EmuIn_MouseUp:
	case EmuIn_Screen:
		break;
	case EmuIn_Start:
		SetTimer(d->hWnd, Screen60hzTimer, 16, NULL);
		break;
	case EmuIn_KeyChar:
		d->dev_ctrl->dat[3] = 0;
		break;
	case EmuIn_Wheel:
		DEVPOKE2(d->dev_mouse, 0xa, 0, 0);
		break;
	}
	d->exec_state = 0;
residual:
	if (d->running && (d->exec_state || d->queue_count))
		ListPushBack(&emus_needing_work, d, work_link);
	InvalidateUxnScreenRect(d);
	/* TODO ^- don't cause a bunch of invalidates if we're only going to draw at 20hz anyway?
	 * Not always a clear win -- it makes it buffer and process more mouse moves and redraw less frequently.
	 * In programs like Left that actually makes it feel less responsive. But it "completes" more mouse move
	 * vector calls.*/
	if (TimeStampNow() - d->last_paint > RepaintTimeLimit) UpdateWindow(d->hWnd);
}


static void ApplyInputEvent(EmuWindow *d, BYTE type, BYTE bits, USHORT x, USHORT y)
{
	Uint16 *pc = &d->box->core.pc;
	switch (type)
	{
	case EmuIn_KeyChar:
		d->dev_ctrl->dat[3] = bits;
		*pc = GETVECTOR(d->dev_ctrl);
		break;
	case EmuIn_CtrlDown: d->dev_ctrl->dat[2] |=  bits; goto run_ctrl;
	case EmuIn_CtrlUp:   d->dev_ctrl->dat[2] &= ~bits; goto run_ctrl;
	case EmuIn_ResetKeys:
		/* If the requested keys held down match the existing, there's nothing more to do. */
		/* Can't skip RunUxn() since we might need to queue more work. TODO could factor out. */
		if (d->dev_ctrl->dat[2] == bits) { *pc = 0; break; }
		d->dev_ctrl->dat[2] = bits;
	run_ctrl:
		*pc = GETVECTOR(d->dev_ctrl);
		break;
	case EmuIn_MouseDown:
		d->dev_mouse->dat[6] |= bits; goto mouse_xy;
	case EmuIn_MouseUp:
		d->dev_mouse->dat[6] &= ~bits;
	mouse_xy:
		DEVPOKE2(d->dev_mouse, 0x2, x, y);
		*pc = GETVECTOR(d->dev_mouse);
		break;
	case EmuIn_Wheel:
		DEVPOKE2(d->dev_mouse, 0xa, x, y);
		*pc = GETVECTOR(d->dev_mouse);
		break;
	case EmuIn_Screen:
		*pc = GETVECTOR(d->dev_screen);
		break;
	case EmuIn_Start:
		*pc = UXN_ROM_OFFSET;
		break;
	}
	d->exec_state = type;
	RunUxn(d);
}

static void SendInputEvent(EmuWindow *d, BYTE type, BYTE bits, USHORT x, USHORT y)
{
	if (!d->running) return;
	if (!d->exec_state && !d->queue_count) ApplyInputEvent(d, type, bits, x, y);
	else /* busy */
	{
		EmuInEvent *evt;
		if (d->queue_count == QUEUE_CAP) return; /* it's too crowded here anyway */
		if (!d->queue_buffer) d->queue_buffer = AllocZeroedOrFail(QUEUE_CAP * sizeof(EmuInEvent));
		evt = d->queue_buffer + (d->queue_first + d->queue_count++) % QUEUE_CAP;
		evt->type = type; evt->bits = bits; evt->x = x; evt->y = y;
	}
}

static void StartVM(EmuWindow *d)
{
	if (LoadROMIntoBox(d->box, d->rom_path))
	{
		d->running = 1;
		SendInputEvent(d, EmuIn_Start, 0, 0, 0);
	}
}

static void ReloadFromROMFile(EmuWindow *d)
{
	PauseVM(d);
	ResetVM(d);
	StartVM(d);
	SynthesizeMouseMoveToCurrent(d); /* Still has a brief flicker of wrong cursor... oh well */
	/* We want to resync the held keys here, but it's not safe to do so, because we get garbage results from GetKeyState() and GetAsyncKeyState() if the open file dialog has just recently been closed by the user. There are also similar issues with syncing key state when reactivating the window by clicking on the title bar while holding modifiers. So forget about it for now.*/
}

static void OpenROMDialog(EmuWindow *d)
{
	TCHAR filename[MAX_PATH]; int filelen;
	OPENFILENAME ofn;
	filename[0] = 0;

	ZeroMemory(&ofn, sizeof ofn);
	ofn.lStructSize = sizeof ofn;
	ofn.hwndOwner = d->hWnd;
	ofn.lpstrFile = filename;
	ofn.nMaxFile = sizeof filename;
	ofn.lpstrFilter = TEXT("Uxn ROM\0*.ROM\0All\0*.*\0");
	ofn.nFilterIndex = 1;
	ofn.lpstrFileTitle = NULL;
	ofn.nMaxFileTitle = 0;
	ofn.lpstrInitialDir = NULL;
	ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;

	if (!GetOpenFileName(&ofn)) return;
	filelen = lstrlen(filename);
	CopyMemory(d->rom_path, filename, (filelen + 1) * sizeof(TCHAR));
	ReloadFromROMFile(d);
}

static LRESULT CALLBACK AboutBoxProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	(void)lParam;
	switch (message)
	{
	case WM_INITDIALOG:
		return TRUE;

	case WM_COMMAND:
		if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)
		{
			EndDialog(hDlg, LOWORD(wParam));
			return TRUE;
		}
		break;
	}
	return FALSE;
}

static LPCSTR EmuWinClass = TEXT("uxn_emu_win");

HWND CreateUxnWindow(HINSTANCE hInst, LPCSTR file)
{
	RECT rect;
	rect.left = 0; rect.top = 0;
	rect.right = rect.left + UXN_DEFAULT_WIDTH;
	rect.bottom = rect.top + UXN_DEFAULT_HEIGHT;
	AdjustWindowRect(&rect, WS_OVERLAPPEDWINDOW, TRUE);
	return CreateWindowEx(WS_EX_APPWINDOW, EmuWinClass, TEXT("Uxn"), WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, rect.right - rect.left, rect.bottom - rect.top, NULL, NULL, hInst, (void *)file);
}

static void CloneWindow(EmuWindow *a)
{
	HWND hWnd = CreateUxnWindow((HINSTANCE)GetWindowLongPtr(a->hWnd, GWLP_HINSTANCE), NULL);
	ShowWindow(hWnd, SW_SHOW);
	PostMessage(hWnd, UXNMSG_BecomeClone, (WPARAM)a, 0);
}

static LRESULT CALLBACK WindowProc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
	EmuWindow *d = (EmuWindow *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
	switch (msg)
	{
		case WM_CREATE:
		{
			LPCSTR filename = ((CREATESTRUCT *)lparam)->lpCreateParams; int filelen;
			emu_window_count++;
			d = AllocZeroedOrFail(sizeof(EmuWindow));
			SetWindowLongPtr(hwnd, GWLP_USERDATA, (LONG_PTR)d);
			DragAcceptFiles(hwnd, TRUE);
			InitEmuWindow(d, hwnd);
			filelen = filename ? lstrlen(filename) : 0;
			if (filelen >= MAX_PATH) OutOfMemory(); /* wrong, better msg */
			if (filelen)
			{
				CopyMemory(d->rom_path, filename, (filelen + 1) * sizeof(TCHAR));
				StartVM(d);
			}
			return 0;
		}
		case WM_CLOSE:
			DestroyWindow(hwnd);
			return 0;
		case WM_DESTROY:
			KillTimer(hwnd, Screen60hzTimer);
			FreeUxnBox(d->box);
			FreeUxnScreen(&d->screen);
			ResetFiler(&d->filer);
			if (d->hBMP) DeleteObject(d->hBMP);
			if (d->hDibDC) DeleteDC(d->hDibDC);
			if (d->wave_out)
			{
				if (d->wave_out->hWaveOut)
				{
					waveOutPause(d->wave_out->hWaveOut);
					waveOutClose(d->wave_out->hWaveOut);
				}
				HeapFree(GetProcessHeap(), 0, d->wave_out);
			}
			ListRemove(&emus_needing_work, d, work_link);
			HeapFree(GetProcessHeap(), 0, d);
			if (!--emu_window_count) PostQuitMessage(0);
			return 0;
		case WM_PAINT:
		{
			PAINTSTRUCT ps; HDC hDC; BITMAPINFO bmi; RECT crect;
			GetClientRect(hwnd, &crect);
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
				/* SIZE_T width = MIN(p->width, sec.dsBm.bmWidth), height = MIN(p->height, sec.dsBm.bmHeight); */
				/* TODO deal with bitmap possibly having padding */
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
			/* One-line version, doesn't need the retained stuff, but probably slower: */
			/* SetDIBitsToDevice(hDC, 0, 0, UXN_DEFAULT_WIDTH, UXN_DEFAULT_HEIGHT, 0, 0, 0, UXN_DEFAULT_HEIGHT, uxn_screen.pixels, &bmi, DIB_RGB_COLORS); */
			/* SetDIBits(d->hDibDC, d->hBMP, 0, UXN_DEFAULT_HEIGHT, d->screen.pixels, &bmi, DIB_RGB_COLORS); */
			SelectObject(d->hDibDC, d->hBMP);
			if (d->viewport_scale == 1)
				BitBlt(hDC, d->viewport_rect.left, d->viewport_rect.top, d->screen.width, d->screen.height, d->hDibDC, 0, 0, SRCCOPY);
			else
				StretchBlt(hDC, d->viewport_rect.left, d->viewport_rect.top, d->viewport_rect.right - d->viewport_rect.left, d->viewport_rect.bottom - d->viewport_rect.top, d->hDibDC, 0, 0, d->screen.width, d->screen.height, SRCCOPY);
			EndPaint(hwnd, &ps);
			d->last_paint = TimeStampNow();
			if (d->needs_audio == 1)
			{
				d->needs_audio = 2;
				SetTimer(d->hWnd, InitAudioTimer, 100, NULL);
				/* Deferring audio init with PostMessage still causes some mild weirdness -- the taskbar icon will sometimes be invisible before showing up. 50ms+ blocking from waveOutOpen is enough to cause that and possibly other issues, so let's defer it even longer with a timer. */
			}
			return 0;
		}
		case WM_GETMINMAXINFO:
		{
			MINMAXINFO *info = (MINMAXINFO *)lparam;
			/* Prevent window from becoming so narrow that the menu bar wraps. */
			/* Could make this dynamically smaller when we add menu bar hiding. */
			info->ptMinTrackSize.x = info->ptMinTrackSize.y = 185;
			return 0;
		}
		case WM_SIZE:
			CalcUxnViewport(d);
			d->needs_clear = 1;
			return 0;

		{
		enum { DN = EmuIn_MouseDown, UP = EmuIn_MouseUp }; unsigned int mode, bits;
		case WM_MOUSEMOVE:   mode = DN; bits = 0x0; goto act_mouse;
		case WM_LBUTTONDOWN: mode = DN; bits = 0x1; goto act_mouse;
		case WM_LBUTTONUP:   mode = UP; bits = 0x1; goto act_mouse;
		case WM_MBUTTONDOWN: mode = DN; bits = 0x2; goto act_mouse;
		case WM_MBUTTONUP:   mode = UP; bits = 0x2; goto act_mouse;
		case WM_RBUTTONDOWN: mode = DN; bits = 0x4; goto act_mouse;
		case WM_RBUTTONUP:   mode = UP; bits = 0x4; goto act_mouse;
		act_mouse:
		{
			POINT mouse; BOOL mouse_in_uxn;
			mouse.x = LOWORD(lparam); mouse.y = HIWORD(lparam);
			mouse_in_uxn = PtInRect(&d->viewport_rect, mouse) && d->running; /* TODO fix unpause without moving mouse */
			SetHostCursorVisible(d, !mouse_in_uxn);
			if (!mouse_in_uxn) break;
			BindPointToLocalUxnScreen(&d->viewport_rect, d->viewport_scale, &mouse); /* could save a GetClientRect call by passing it optionally... */
			SendInputEvent(d, mode, bits, (USHORT)mouse.x, (USHORT)mouse.y);
			return 0;
		}
		case WM_MOUSEWHEEL:
		{
			POINT mouse; short zDelta = (short)HIWORD(wparam);
			mouse.x = LOWORD(lparam); mouse.y = HIWORD(lparam);
			if (!PtInRect(&d->viewport_rect, mouse)) break;
			/* could set mouse x,y pos here if we wanted to */
			/* TODO no x axis scrolling yet */
			/* TODO accumulate error from division */
			SendInputEvent(d, EmuIn_Wheel, 0, 0, (Uint16)(-zDelta / 120));
			return 0;
		}
		}
		case WM_ACTIVATE: /* When losing focus, clear any held keys so that they aren't stuck down */
			if (LOWORD(wparam) == WA_INACTIVE)
		case WM_ENTERMENULOOP:
				SendInputEvent(d, EmuIn_ResetKeys, 0, 0, 0);
			/* This doesn't fix starting to hold a key down before activating this window, but that seems to be hard due to garbage GetKeyState() after a dialog closes or when clicking on the window title bar. */
			break;
		case WM_SYSCHAR: /* Suppress Alt+letter menu accelerators when VM is focused and running */
			if (d->running && !d->host_cursor) return 0;
			break;
		case WM_KEYDOWN: case WM_SYSKEYDOWN: case WM_KEYUP: case WM_SYSKEYUP:
		{
			int up = (int)lparam & 1 << 31, was_down = lparam & 1 << 30, bits = 0; TCHAR keyChar;
			switch ((int)wparam)
			{
			case VK_CONTROL: bits = 0x01; break;
			case VK_MENU:    bits = 0x02; break;
			case VK_SHIFT:   bits = 0x04; break;
			case VK_HOME:    bits = 0x08; goto allow_key_repeat;
			case VK_UP:      bits = 0x10; goto allow_key_repeat;
			case VK_DOWN:    bits = 0x20; goto allow_key_repeat;
			case VK_LEFT:    bits = 0x40; goto allow_key_repeat;
			case VK_RIGHT:   bits = 0x80; goto allow_key_repeat;

			/* Add quick debug keys here */
			/* case VK_F12: if (!up) {  } return 0; */
			default: goto other_vkey;
			}
			if (!up && was_down) return 0;
		allow_key_repeat:
			SendInputEvent(d, up ? EmuIn_CtrlUp : EmuIn_CtrlDown, bits, 0, 0);
			return 0;
		other_vkey:
			if (up || !(keyChar = MapVirtualKey(wparam, MAPVK_VK_TO_CHAR))) break;
			/* Holding Alt or Ctrl causes characters to appear upper case, so if shift isn't held, turn 'em lower. */
			if (keyChar >= 'A' && keyChar <= 'Z' && !(!(GetKeyState(VK_SHIFT) & 0x8000) ^ !(GetKeyState(VK_CAPITAL)))) keyChar += 0x20;
			/* Disallow control characters except tab, newline, etc. */
			if (keyChar < 32 && keyChar != 8 && keyChar != 9 && keyChar != 10 && keyChar != 13 && keyChar != 27) break;
			SendInputEvent(d, EmuIn_KeyChar, (BYTE)keyChar, 0, 0);
			return 0;
		}

		case WM_NCMOUSEMOVE:
		case WM_MOUSELEAVE:
			SetHostCursorVisible(d, TRUE);
			break;
		case WM_TIMER:
			switch (wparam)
			{
			case Screen60hzTimer: SendInputEvent(d, EmuIn_Screen, 0, 0, 0); return 0;
			case InitAudioTimer:
				KillTimer(hwnd, InitAudioTimer);
				if (!d->wave_out) InitWaveOutAudio(d);
				return 0;
			}
			break;
		case WM_DROPFILES:
			if (!DragQueryFile((HDROP)wparam, 0, d->rom_path, MAX_PATH)) return 0;
			ReloadFromROMFile(d);
			return 0;

		case WM_COMMAND:
			switch (LOWORD(wparam))
			{
			case IDM_ABOUT:
				DialogBox((HINSTANCE)GetWindowLongPtr(hwnd, GWLP_HINSTANCE), MAKEINTRESOURCE(IDD_ABOUTBOX), hwnd, AboutBoxProc);
				return 0;
			case IDM_EXIT: PostQuitMessage(0); return 0;
			case IDM_OPENROM: OpenROMDialog(d); return 0;
			case IDM_CLONEWINDOW: CloneWindow(d); return 0;
			case IDM_TOGGLEZOOM: d->viewport_scale = d->viewport_scale == 1 ? 2 : 1; RefitEmuWindow(d); return 0;
			case IDM_RELOAD: ReloadFromROMFile(d); return 0;
			case IDM_CLOSEWINDOW: PostMessage(hwnd, WM_CLOSE, 0, 0); return 0;
			case IDM_PAUSE: if (d->running) PauseVM(d); else UnpauseVM(d); return 0;
				break;
			}
			break;

		case UXNMSG_ContinueExec:
			if (!d->running) return 0;
			if (d->exec_state) RunUxn(d); /* Unfinished vector execution */
			else if (d->queue_count) /* Buffered events */
			{
				EmuInEvent *evt = &d->queue_buffer[d->queue_first];
				d->queue_first = (d->queue_first + 1) % QUEUE_CAP;
				d->queue_count--;
				ApplyInputEvent(d, evt->type, evt->bits, evt->x, evt->y);
			}
			return 0;
		case UXNMSG_BecomeClone:
		{
			EmuWindow *b = (EmuWindow *)wparam;
			d->box->core.fault_code = b->box->core.fault_code;
			d->exec_state = b->exec_state;
			CopyMemory(&d->box->work_stack, &b->box->work_stack, sizeof(Stack) * 2);
			CopyMemory(d->box->device_memory, b->box->device_memory, sizeof d->box->device_memory);
			CopyMemory((char *)(d->box + 1), (char *)(b->box + 1), UXN_RAM_SIZE);
			CopyMemory(d->screen.palette, b->screen.palette, sizeof d->screen.palette);
			SetUxnScreenSize(&d->screen, b->screen.width, b->screen.height);
			d->viewport_scale = b->viewport_scale;
			RefitEmuWindow(d);
			CopyMemory(d->screen.bg, b->screen.bg, d->screen.width * d->screen.height * 2);
			CopyMemory(d->rom_path, b->rom_path, MAX_PATH);
			/* can't copy filer state */
			UnpauseVM(d);
			UpdateWindow(d->hWnd);
			return 0;
		}
	}
	return DefWindowProc(hwnd, msg, wparam, lparam);
}

int CALLBACK WinMain(HINSTANCE instance, HINSTANCE prev_instance, LPSTR command_line, int show_code)
{
	WNDCLASSEX wc; HWND hWin;
	MSG msg; HACCEL hAccel;
	(void)command_line; (void)prev_instance;
	QueryPerformanceFrequency(&_perfcount_freq);
	ExecutionTimeLimit = _perfcount_freq.QuadPart / 20;
	ZeroMemory(&wc, sizeof wc);
	wc.hInstance = instance;
	wc.cbSize = sizeof wc;
	wc.lpfnWndProc = WindowProc;
	wc.lpszClassName = EmuWinClass;
	wc.lpszMenuName = MAKEINTRESOURCE(IDC_UXN32);
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hIcon = LoadIcon(instance, (LPCTSTR)IDI_UXN32); /* use this one */
	/* wc.hIconSm = LoadIcon(instance, (LPCTSTR)IDI_UXN32); */
	wc.style = CS_HREDRAW | CS_VREDRAW;
	/* wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH); */
	RegisterClassEx(&wc);
	hAccel = LoadAccelerators(instance, (LPCSTR)IDC_UXN32);

	hWin = CreateUxnWindow(instance, TEXT("launcher.rom"));
	ShowWindow(hWin, show_code);

	for (;;)
	{
		while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
		{
			if (msg.message == WM_QUIT) goto quitting;
			if (TranslateAccelerator(msg.hwnd, hAccel, &msg)) continue;
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
		while (emus_needing_work.front)
		{
			EmuWindow *d = ListPopFront(&emus_needing_work, EmuWindow, work_link);
			PostMessage(d->hWnd, UXNMSG_ContinueExec, 0, 0);
		}
		WaitMessage();
	}
quitting:
	return 0;
}
