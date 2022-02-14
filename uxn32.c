#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include "resource.h"
#include "core32.h"
#include <Windows.h>
#include <shellapi.h>
#include <Shlwapi.h>
#include <commdlg.h>
#include <CommCtrl.h>
#include <mmsystem.h>

#pragma comment(lib, "user32.lib")
#pragma comment(lib, "gdi32.lib")
#pragma comment(lib, "shell32.lib")
#pragma comment(lib, "shlwapi.lib")
#pragma comment(lib, "comdlg32.lib")
#pragma comment(lib, "comctl32.lib")
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
#define DEBUG_CHECK(a) ((void)0)
#else
#define DEBUG_CHECK(a) (!(a) ? DebugBreak() : (void)0)
#endif

#define UXN_DEFAULT_WIDTH (64 * 8)
#define UXN_DEFAULT_HEIGHT (40 * 8)
#define UXN_RAM_SIZE (0x10000u)
#define UXN_ROM_OFFSET (0x0100u)
#define UXN_SAMPLE_RATE 44100
#define UXN_VOICES 4

#define DEVPEEK(d, o, x) ((o) = ((d)->dat[(x)] << 8) + (d)->dat[(x) + 1])
#define DEVPOKE(d, a, v) ((d)->dat[(a)] = (v) >> 8, (d)->dat[(a) + 1] = (v))
#define DEVPEEK2(d, o1, o2, a) (DEVPEEK(d, o1, a), DEVPEEK(d, o2, a + 2))
#define DEVPOKE2(d, a, v1, v2) (DEVPOKE(d, a, v1), DEVPOKE(d, a + 2, v2))
#define GETVECTOR(d) ((d)->dat[0] << 8 | (d)->dat[1])

static LARGE_INTEGER _perfcount_freq;
static LONGLONG ExecutionTimeLimit;
#define RepaintTimeLimit ExecutionTimeLimit
static LPCSTR EmuWinClass = TEXT("uxn_emu_win"), ConsoleWinClass = TEXT("uxn_console_win"), EditWinClass = TEXT("EDIT");
static LPCSTR BeetbugWinClass = TEXT("uxn_beetbug_win");

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

enum {FromLeft, FromTop, FromRight, FromBottom};
static void CutRect(RECT *in, int dir, int length, RECT *out)
{
	LONG b = dir + 2 & 3, c = dir + 1 & 3, d = dir + 3 & 3, in_a = (&in->left)[dir];
	/* There's probably a smarter way to do this with unsigned math */
	LONG avail = dir & 2 ? in_a - (&in->left)[b] : (&in->left)[b] - in_a;
	if (avail > length) avail = length;
	if (dir & 2) avail = -avail;
	(&out->left)[b] = (&in->left)[dir] = in_a + avail;
	(&out->left)[dir] = in_a;
	(&out->left)[c] = (&in->left)[c];
	(&out->left)[d] = (&in->left)[d];
}
static BOOL MoveWindowRect(HWND hWnd, RECT const *to, BOOL bRepaint)
{
	return MoveWindow(hWnd, to->left, to->top, to->right - to->left, to->bottom - to->top, bRepaint);
}
static void CutRectForWindow(RECT *prect, int dir, int length, HWND window)
{
	RECT r; CutRect(prect, dir, length, &r);
	MoveWindowRect(window, &r, TRUE);
}

typedef struct UxnBox
{
	void *user;
	Uxn core;
	Stack work_stack, ret_stack;
	Uint8 device_memory[256];
} UxnBox;
typedef struct UxnVoice
{
	Uint32 count, advance, period, age, a, d, s, r;
	Uint16 wave_base, i, len;
	Sint8 volume[2];
	Uint8 repeat;
} UxnVoice;
typedef struct UxnWaveOut
{
	HWAVEOUT hWaveOut;
	WAVEHDR waveHdrs[2];
	SHORT *sampleBuffers[2];
	BYTE which_buffer;
} UxnWaveOut;
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

enum { TimerID_Screen60hz = 1, TimerID_InitAudio };
enum { UXNMSG_ContinueExec = WM_USER, UXNMSG_BecomeClone };
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
	EmuIn_Console,
	EmuIn_Start
};
typedef struct EmuInEvent
{
	BYTE type, bits;
	USHORT x, y;
} EmuInEvent;

/* Maximum number of queued input events for emulated machines */
#define QUEUE_CAP 256

typedef struct EmuWindow
{
	UxnBox *box;
	Device *dev_screen, *dev_mouse, *dev_ctrl, *dev_audio0;
	HWND hWnd, consoleHWnd, beetbugHWnd;
	HBITMAP hBMP;
	HDC hDibDC;
	SIZE dib_dims;

	BYTE running, exec_state, host_cursor;

	EmuInEvent *queue_buffer;
	USHORT queue_count, queue_first;
	ListLink work_link;
	LONGLONG last_paint;

	RECT viewport_rect;
	LONG viewport_scale; /* really only need 1 bit for this... */
	UxnScreen screen;
	UxnFiler filer;
	UxnVoice synth_voices[UXN_VOICES];
	UxnWaveOut *wave_out;

	BYTE needs_audio; /* 3 states, 0: not needed, 1: plz init, 2: init started */
	TCHAR rom_path[MAX_PATH];
} EmuWindow;

typedef struct ConWindow
{
	HWND outHWnd, inHWnd;
	BYTE has_newline;
} ConWindow;

typedef struct BeetbugWin {
	EmuWindow *emu;
	HWND hWnd, hDisList, hHexList, hWrkStack, hRetStack, hDevMem,
	     hStatus, hBigStepBtn, hStepBtn, hPauseBtn,
		 hStackPushBtn0, hStackPushBtn1, hStackPopBtn0, hStackPopBtn1;
	USHORT sbar_pc;
	RECT rcBlank, rcWstLabel, rcRstLabel, rcDevMemLabel;
	BYTE sbar_play_mode, sbar_input_event;
} BeetbugWin;

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

static HFONT GetSmallFixedFont(void)
{
	static HFONT hFont;
	if (!hFont) hFont = CreateFont(
		8, 6, 0, 0, 0, 0, 0, 0, OEM_CHARSET, OUT_RASTER_PRECIS,
		CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FIXED_PITCH, TEXT("Terminal"));
	return hFont;
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

static EmuWindow * EmuOfDevice(Device *d) /* TODO these are kinda dumb, clean when making Devices better */
{
	UxnBox *box = OUTER_OF(d->u, UxnBox, core);
	return (EmuWindow *)box->user;
}

static UxnScreen * ScreenOfDevice(Device *d)
{
	UxnBox *box = OUTER_OF(d->u, UxnBox, core);
	return &((EmuWindow *)box->user)->screen;
}

static Uint8 DevIn_Screen(Device *d, Uint8 port)
{
	UxnScreen *screen = ScreenOfDevice(d);
	switch (port)
	{
	case 0x2: return screen->width >> 8;
	case 0x3: return screen->width;
	case 0x4: return screen->height >> 8;
	case 0x5: return screen->height;
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

static void DevOut_Screen(Device *d, Uint8 port)
{
	UxnScreen *screen = ScreenOfDevice(d);
	Uxn *u = d->u; /* TODO */
	switch (port)
	{
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
	case 0xE:
	{
		LONG x, y, width = screen->width, layer = d->dat[0xE] & 0x40;
		Uint8 *pixels = layer ? screen->fg : screen->bg;
		DEVPEEK2(d, x, y, 0x8);
		if (x < width && y < screen->height) /* poke pixel */
			pixels[x + y * width] = d->dat[0xE] & 0x3;
		if (d->dat[0x6] & 0x01) DEVPOKE(d, 0x8, x + 1); /* auto x+1 */
		if (d->dat[0x6] & 0x02) DEVPOKE(d, 0xA, y + 1); /* auto y+1 */
		break;
	}
	case 0xF:
	{
		UINT x, y, addr, tmp, advnc = d->dat[0x6], sprite = d->dat[0xF];
		UINT twobpp = !!(sprite & 0x80);
		Uint8 *layer_pixels = (sprite & 0x40) ? screen->fg : screen->bg;
		DEVPEEK2(d, x, y, 0x8);
		DEVPEEK(d, addr, 0xC);
		DrawUxnSprite(screen, layer_pixels, x, y, &u->ram[addr], sprite & 0xF, sprite & 0x10, sprite & 0x20, twobpp);
		/* auto addr+length */
		if (advnc & 0x04) { tmp = addr + 8 + twobpp * 8; DEVPOKE(d, 0xC, tmp); }
		if (advnc & 0x01) { tmp = x + 8; DEVPOKE(d, 0x8, tmp); } /* auto x+8 */
		if (advnc & 0x02) { tmp = y + 8; DEVPOKE(d, 0xA, tmp); } /* auto y+8 */
		break;
	}
	}
}

static Uint8 DevIn_System(Device *d, Uint8 port)
{
	switch (port)
	{
	case 0x2: return d->u->wst->ptr;
	case 0x3: return d->u->rst->ptr;
	}
	return d->dat[port];
}

static void DevOut_System(Device *d, Uint8 port)
{
	switch (port)
	{
	case 0x2: d->u->wst->ptr = d->dat[port]; return;
	case 0x3: d->u->rst->ptr = d->dat[port]; return;
	case 0xE: d->u->fault_code = 0xFF; return;
	}

	if (port > 0x7 && port < 0xE) /* modify palette */
	{
		UxnScreen *p = ScreenOfDevice(d);
		Uint8* addr = &d->dat[0x8];
		int i, shift;
		for (i = 0, shift = 4; i < 4; ++i, shift ^= 4)
		{
			Uint8
				r = (addr[0 + i / 2] >> shift) & 0x0F,
				g = (addr[2 + i / 2] >> shift) & 0x0F,
				b = (addr[4 + i / 2] >> shift) & 0x0F;
			p->palette[i] = 0x0F000000 | r << 16 | g << 8 | b;
			p->palette[i] |= p->palette[i] << 4;
		}
	}
}

static Uint8 DevIn_Date(Device *d, Uint8 port)
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
	case 0xA: return GetTimeZoneInformation(&zone) == 2;
	}
	return d->dat[port];
}

static void ResetFiler(UxnFiler *f)
{
	if (f->hFile != INVALID_HANDLE_VALUE) { CloseHandle(f->hFile); f->hFile = INVALID_HANDLE_VALUE; }
	if (f->hFind != INVALID_HANDLE_VALUE) { FindClose(f->hFind); f->hFind = INVALID_HANDLE_VALUE; }
	f->state = FileDevState_Init;
}

static UxnFiler * FilerOfDevice(Device *d)
{
	UxnBox *box = OUTER_OF(d->u, UxnBox, core);
	return &((EmuWindow *)box->user)->filer;
}

/* Returns 0 on error, including not enough space. Will not write to the dst buffer on error.
 * dst_len should be at least strlen(display_name) + 7 or it will definitely not work. */
static DWORD PrintDirListRow(char *dst, DWORD dst_len, char *display_name, DWORD file_size_high, DWORD file_size_low, DWORD is_dir)
{
	int written; int ok = !file_size_high && file_size_low < 0x10000; char tmp[1024 + 1];
	if (dst_len > sizeof tmp) dst_len = sizeof tmp;
	if (is_dir || !ok) written = wsprintfA(tmp, ok ? "---- %s\n" : "???? %s\n", display_name);
	else written = wsprintfA(tmp, "%04x %s\n", (unsigned int)file_size_low, display_name);
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
	for (i = 0;; i++)
	{
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
	if (f->state != FileDevState_Reading)
	{
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
		}
		else
		{
			f->hFile = CreateFile(filepath, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
			if (f->hFile == INVALID_HANDLE_VALUE) return 0;
		}
		f->state = FileDevState_Reading;
	}
	if (f->hFind != INVALID_HANDLE_VALUE)
	{
		for (;;)
		{
			/* DWORD copy = WideCharToMultiByte(1252, 0, ) */
			DWORD written;
			if (find_data->cFileName[0] == '.' && find_data->cFileName[1] == 0) goto next;
			written = PrintDirListRow(dst, dst_len, find_data->cFileName, find_data->nFileSizeHigh, find_data->nFileSizeLow, find_data->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
			if (!written) break;
			dst += written; result += written; dst_len -= written;
		next:
			if (!FindNextFile(f->hFind, find_data)) goto done_dir;
		}
		if (result == 0) done_dir: ResetFiler(f); /* If 0 bytes were written, always end iteration */
	}
	else if (f->hFile != INVALID_HANDLE_VALUE)
	{
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
	if (f->state != FileDevState_Writing)
	{
		int append = flags & 0x01;
		ResetFiler(f);
		f->hFile = CreateFile(f->path, GENERIC_WRITE, FILE_SHARE_READ, NULL, append ? OPEN_ALWAYS : CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	}
	if (f->hFile == INVALID_HANDLE_VALUE) return 0;
	if (!WriteFile(f->hFile, src, src_len, &written, NULL))
	{
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
	if (!ok) info.nFileSizeHigh = info.nFileSizeLow = (DWORD)-1;
	CopyMemory(path_tmp, f->path, f->pathlen + 1);
	PathStripPathA(path_tmp);
	written = PrintDirListRow(dst, dst_len, path_tmp, info.nFileSizeHigh, info.nFileSizeLow, dir);
	ResetFiler(f);
	return written;
}

static DWORD FileDevDelete(UxnFiler *f)
{
	DWORD result;
	ResetFiler(f);
	if (!f->pathlen) return 0;
	result = PathIsDirectoryA(f->path) ? RemoveDirectoryA(f->path) : DeleteFileA(f->path);
	return result ? 0 : 1;
}

#define NOTE_PERIOD (UXN_SAMPLE_RATE * 0x4000 / 11025)
#define ADSR_STEP (UXN_SAMPLE_RATE / 0xF)

static const Uint32 advances[12] = {
	0x80000, 0x879C8, 0x8FACD, 0x9837F, 0xA1451, 0xAADC1,
	0xB504F, 0xBFC88, 0xCB2FF, 0xD7450, 0xE411F, 0xF1A1C
};

static INT VoiceEnvelope(UxnVoice *c, Uint32 age)
{
	if (!c->r)      return 0x0888;
	if (age < c->a) return 0x0888 * age / c->a;
	if (age < c->d) return 0x0444 * (2 * c->d - c->a - age) / (c->d - c->a);
	if (age < c->s) return 0x0444;
	if (age < c->r) return 0x0444 * (c->r - age) / (c->r - c->s);
	c->advance = 0;
	return 0x0000;
}

static int VoiceRender(UxnVoice *c, Uint8 *uxn_ram, SHORT *out, SHORT *end)
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
		s = (Sint8)(uxn_ram[(c->wave_base + c->i) % UXN_RAM_SIZE] + 0x80) * VoiceEnvelope(c, c->age++);
		*out++ += s * c->volume[0] / (0x180 * 2); /* Original: / (0x180 * 1) */
		*out++ += s * c->volume[1] / (0x180 * 2); /* Temporarily make this quieter until we add volume slider */
	}
	/* if (!c->advance) audio_is_finished(c); */
	return 1;
}

static void VoiceStart(UxnVoice *c, Uint16 adsr, Uint8 pitch)
{
	if (!(pitch < 108 && c->len))
	{
		c->advance = 0;
		return;
	}
	c->advance = advances[pitch % 12] >> (8 - pitch / 12);
	c->a = ADSR_STEP * (adsr >> 12);
	c->d = ADSR_STEP * (adsr >> 8 & 0xF) + c->a;
	c->s = ADSR_STEP * (adsr >> 4 & 0xF) + c->d;
	c->r = ADSR_STEP * (adsr >> 0 & 0xF) + c->s;
	c->age = 0;
	c->i = 0;
	if (c->len <= 0x100) /* single cycle mode */
		c->period = NOTE_PERIOD * 337 / 2 / c->len;
	else /* sample repeat mode */
		c->period = NOTE_PERIOD;
}

static Uint8 VoiceCalcVU(UxnVoice *c)
{
	INT sum[2] = {0, 0}, i;
	if (!c->advance || !c->period) return 0;
	for (i = 0; i < 2; i++)
	{
		if (!c->volume[i]) continue;
		sum[i] = 1 + VoiceEnvelope(c, c->age) * c->volume[i] / 0x800;
		if (sum[i] > 0xF) sum[i] = 0xF;
	}
	return (sum[0] << 4) | sum[1];
}

#define AUDIO_BUF_SAMPLES 2048

static void WriteOutSynths(EmuWindow *d)
{
	UxnWaveOut *wave_out = d->wave_out;
	WAVEHDR *hdr = &wave_out->waveHdrs[wave_out->which_buffer];
	SHORT *samples = wave_out->sampleBuffers[wave_out->which_buffer];
	MMRESULT res; int i, still_running;
	if (!wave_out->hWaveOut) return;
	wave_out->which_buffer = 1 - wave_out->which_buffer;
	ZeroMemory(hdr, sizeof(WAVEHDR));
	hdr->dwBufferLength = AUDIO_BUF_SAMPLES * 2 * sizeof(SHORT);
	hdr->lpData = (LPSTR)samples;
	ZeroMemory(samples, AUDIO_BUF_SAMPLES * 2 * sizeof(SHORT));
	for (i = still_running = 0; i < UXN_VOICES; i++)
		still_running |= VoiceRender(&d->synth_voices[i], d->box->core.ram, samples, samples + AUDIO_BUF_SAMPLES * 2);
	res = waveOutPrepareHeader(wave_out->hWaveOut, hdr, sizeof(WAVEHDR));
	res = waveOutWrite(wave_out->hWaveOut, hdr, sizeof(WAVEHDR));
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

static Uint8 DevIn_Audio(Device *dev, Uint8 port)
{
	UxnBox *box = OUTER_OF(dev->u, UxnBox, core); /* TODO you know this mess */
	EmuWindow *win = (EmuWindow *)box->user;
	UxnVoice *voice = &win->synth_voices[dev - win->dev_audio0];
	if (!win->wave_out || !win->wave_out->hWaveOut) return dev->dat[port];
	switch (port)
	{
	case 0x4: return VoiceCalcVU(voice);
	case 0x2: DEVPOKE(dev, 0x2, voice->i); break;
	}
	return dev->dat[port];
}

static void DevOut_Audio(Device *dev, Uint8 port)
{
	UxnBox *box = OUTER_OF(dev->u, UxnBox, core); /* TODO you know this mess */
	EmuWindow *win = (EmuWindow *)box->user;
	UxnVoice *voice = &win->synth_voices[dev - win->dev_audio0];
	Uint16 adsr;
	if (port != 0xF) return;
	DEVPEEK(dev, adsr, 0x8);
	DEVPEEK(dev, voice->len, 0xA);
	DEVPEEK(dev, voice->wave_base, 0xC);
	voice->volume[0] = dev->dat[0xE] >> 4;
	voice->volume[1] = dev->dat[0xE] & 0xF;
	voice->repeat = !(dev->dat[0xF] & 0x80);
	VoiceStart(voice, adsr, dev->dat[0xF] & 0x7F);
	/* Defer initializing audio until after at least one paint event, because the window might not be visible yet, and this can cause a 50ms+ freeze, increasing the delay before the window is shown. */
	if (!win->needs_audio) win->needs_audio = 1;
}

static void DevOut_File(Device *d, Uint8 port)
{
	DWORD result = 0, /* next inits suppress msvc warning */ out_len = 0; char *out = 0;
	UxnFiler *f = FilerOfDevice(d);
	Uxn *u = d->u; /* TODO */
	switch (port) /* These need write location and size */
	{
	int peek_at; DWORD dst, avail;
	case 0x5: peek_at = 0x4; goto calc;
	case 0xD: peek_at = 0xC; goto calc;
	case 0xF: peek_at = 0xE; goto calc;
	calc:
		DEVPEEK(d, dst, peek_at);
		DEVPEEK(d, out_len, 0xA);
		avail = UXN_RAM_SIZE - dst;
		if (out_len > avail) out_len = avail;
		out = (char *)u->ram + dst;
	}
	switch (port)
	{
	case 0x5: result = FileDevStat(f, out, out_len); goto result;
	case 0x6: result = FileDevDelete(f); goto result;
	case 0x9: result = 0; FileDevPathChange(d); goto result;
	case 0xD: result = FileDevRead(f, out, out_len); goto result;
	case 0xF: result = FileDevWrite(f, out, out_len, d->dat[0x7]); goto result;
	}
	return;
result:
	DEVPOKE(d, 0x2, result);
}

static void CreateConsoleWindow(EmuWindow *emu)
{
	DWORD exStyle = WS_EX_TOOLWINDOW, wStyle = WS_SIZEBOX | WS_SYSMENU;
	RECT rect; rect.left = 0; rect.top = 0; rect.right = 200; rect.bottom = 150;
	AdjustWindowRectEx(&rect, wStyle, FALSE, exStyle);
	emu->consoleHWnd = CreateWindowEx(exStyle, ConsoleWinClass, TEXT("Console"), wStyle, CW_USEDEFAULT, CW_USEDEFAULT, rect.right - rect.left, rect.bottom - rect.top, emu->hWnd, NULL, (HINSTANCE)GetWindowLongPtr(emu->hWnd, GWLP_HINSTANCE), (void *)NULL);
}

static void DevOut_Console(Device *dev, Uint8 port)
{
	char c[4]; EmuWindow *emu = EmuOfDevice(dev); ConWindow *con; int i = 0, len;
	if (port < 0x8) return;
	if (!emu->consoleHWnd) CreateConsoleWindow(emu);
	if (!IsWindowVisible(emu->consoleHWnd)) ShowWindow(emu->consoleHWnd, SW_SHOWNOACTIVATE);
	con = (ConWindow *)GetWindowLongPtr(emu->consoleHWnd, GWLP_USERDATA);
	if (con->has_newline) c[i++] = '\r', c[i++] = '\n', con->has_newline = 0;
	if ((c[i] = dev->dat[port]) == '\n') con->has_newline = 1;
	else i++;
	c[i] = 0;
	len = GetWindowTextLength(con->outHWnd);
	SendMessage(con->outHWnd, EM_SETSEL, len, len);
	SendMessage(con->outHWnd, EM_REPLACESEL, 0, (LPARAM)c);
}

static BOOL LoadROMIntoBox(UxnBox *box, LPCSTR filename)
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

static Uint8 DevIn_Dummy(Device *d, Uint8 port) { return d->dat[port]; }
static void  DevOut_Dummy(Device *d, Uint8 port) { (void)d;(void)port; }

static void InitEmuWindow(EmuWindow *d, HWND hWnd)
{
	char *main_ram; Device *dev;
	UxnBox *box = (UxnBox *)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(UxnBox) + UXN_RAM_SIZE);
	if (!box) OutOfMemory();
	main_ram = (char *)(box + 1);
	box->user = d;
	box->core.ram = (Uint8 *)main_ram;
	box->core.wst = &box->work_stack;
	box->core.rst = &box->ret_stack;
#define DEVICE_AT(portnum, in_fn, out_fn) (dev = box->core.dev + portnum, dev->u = &box->core, dev->dei = in_fn, dev->deo = out_fn, dev->dat = box->device_memory + portnum * 0x10)
	/* System   */ DEVICE_AT(0x0, DevIn_System, DevOut_System);
	/* Console  */ DEVICE_AT(0x1, DevIn_Dummy,  DevOut_Console);
	/* Screen   */ DEVICE_AT(0x2, DevIn_Screen, DevOut_Screen);  d->dev_screen = dev;
	/* Audio0   */ DEVICE_AT(0x3, DevIn_Audio,  DevOut_Audio);   d->dev_audio0 = dev;
	/* Audio1   */ DEVICE_AT(0x4, DevIn_Audio,  DevOut_Audio);
	/* Audio2   */ DEVICE_AT(0x5, DevIn_Audio,  DevOut_Audio);
	/* Audio3   */ DEVICE_AT(0x6, DevIn_Audio,  DevOut_Audio);
	/* Unused   */ DEVICE_AT(0x7, DevIn_Dummy,  DevOut_Dummy);
	/* Control  */ DEVICE_AT(0x8, DevIn_Dummy,  DevOut_Dummy);   d->dev_ctrl = dev;
	/* Mouse    */ DEVICE_AT(0x9, DevIn_Dummy,  DevOut_Dummy);   d->dev_mouse = dev;
	/* File     */ DEVICE_AT(0xA, DevIn_Dummy,  DevOut_File);
	/* Datetime */ DEVICE_AT(0xB, DevIn_Date,   DevOut_Dummy);
	/* Unused   */ DEVICE_AT(0xC, DevIn_Dummy,  DevOut_Dummy);
	/* Unused   */ DEVICE_AT(0xD, DevIn_Dummy,  DevOut_Dummy);
	/* Unused   */ DEVICE_AT(0xE, DevIn_Dummy,  DevOut_Dummy);
	/* Unused   */ DEVICE_AT(0xF, DevIn_Dummy,  DevOut_Dummy);
#undef DEVICE_AT
	d->box = box;
	d->host_cursor = TRUE;
	d->hWnd = hWnd; /* TODO cleanup reorder these assignments */
	d->viewport_scale = 1;
	SetUxnScreenSize(&d->screen, UXN_DEFAULT_WIDTH, UXN_DEFAULT_HEIGHT);
	d->filer.hFile = d->filer.hFind = INVALID_HANDLE_VALUE;
}

static void FreeUxnBox(UxnBox *box)
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
	/* Tnstead of factoring out the event code to a function and having the WindowProc call it,
	 * this is a temp hacky solution, because I still haven't made up my mind about how it should look. */
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
	KillTimer(d->hWnd, TimerID_Screen60hz);
	SetHostCursorVisible(d, TRUE);
	ListRemove(&emus_needing_work, d, work_link);
}

static void UnpauseVM(EmuWindow *d)
{
	if (d->running) return;
	d->queue_count = 0;
	d->running = 1;
	SetTimer(d->hWnd, TimerID_Screen60hz, 16, NULL);
	if (d->exec_state) ListPushBack(&emus_needing_work, d, work_link);
	SynthesizeMouseMoveToCurrent(d); /* Runs async for now */
	/* Syncing held keys isn't so easy... */
}

static void OpenBeetbugWindow(EmuWindow *emu)
{
	if (!emu->beetbugHWnd)
	{
		emu->beetbugHWnd = CreateWindowEx(
			0, BeetbugWinClass, TEXT("Beetbug"), WS_OVERLAPPEDWINDOW,
			CW_USEDEFAULT, CW_USEDEFAULT, 480, 395,
			emu->hWnd, NULL, (HINSTANCE)GetWindowLongPtr(emu->hWnd, GWLP_HINSTANCE), emu);
	}
	if (!IsWindowVisible(emu->beetbugHWnd)) ShowWindow(emu->beetbugHWnd, SW_SHOW);
}

static void BeetbugAutoScrollStacks(BeetbugWin *dbg)
{
	int pp = ListView_GetCountPerPage(dbg->hWrkStack), pad = pp / 3, top, i, s;
	for (i = 0; i < 2; i++)
	{
		top = ListView_GetTopIndex((&dbg->hWrkStack)[i]);
		s = (&dbg->emu->box->core.wst)[i]->ptr; /* actually points to 1 past the last value */
		if (s-- == 0 || s >= top && s < top + pp) continue;
		ListView_EnsureVisible((&dbg->hWrkStack)[i], MAX(0, s - pp), FALSE);
		ListView_EnsureVisible((&dbg->hWrkStack)[i], MIN(255, s + pad), FALSE);
	}
}

static void ShowBeetbugInstruction(EmuWindow *emu, USHORT address)
{
	BeetbugWin *dbg; int pad_rows;
	OpenBeetbugWindow(emu);
	dbg = (BeetbugWin *)GetWindowLongPtr(emu->beetbugHWnd, GWLP_USERDATA);
	pad_rows = ListView_GetCountPerPage(dbg->hDisList) / 3;
	ListView_EnsureVisible(dbg->hDisList, ((UINT)address - pad_rows) % UXN_RAM_SIZE, FALSE); /* TODO probably wasteful */
	ListView_EnsureVisible(dbg->hDisList, (address + pad_rows) % UXN_RAM_SIZE, FALSE);
	BeetbugAutoScrollStacks(dbg);
}

/* TODO there's something fancy we should do with the loop to make it tell if it ran out or not by return value, returning 0 when limit is 0 means we might have succeeded in reaching the null instruction on the last allowed step, so we need to do something else */
/* Pass non-zero steps to use fixed-stepping mode. Pass 0 to use normal run mode with an automatic number of steps. */
static void RunUxn(EmuWindow *d, UINT steps)
{
	UINT res; Uxn *u = &d->box->core;
	LONGLONG total = 0, t_a, t_b, t_delta;
	int instr_interrupts = 0;
	if (!u->pc) goto completed;
	t_a = TimeStampNow();
	for (;;)
	{
		res = UxnExec(&d->box->core, steps ? steps : 100000); /* about 1900 usecs on good hardware */
		instr_interrupts++;
		t_b = TimeStampNow();
		t_delta = t_b - t_a;
		if (u->fault_code) goto died;
		if (res != 0) { total += t_delta; goto completed; }
		if (t_delta > ExecutionTimeLimit || steps) { total += t_delta; goto residual; }
		/* total will include some non-Uxn work, but close enough */
	}
died:
	PauseVM(d);
	InvalidateUxnScreenRect(d);
	ShowBeetbugInstruction(d, u->pc);
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
	case EmuIn_Console:
		break;
	case EmuIn_Start:
		SetTimer(d->hWnd, TimerID_Screen60hz, 16, NULL);
		break;
	case EmuIn_KeyChar:
		d->dev_ctrl->dat[3] = 0;
		break;
	case EmuIn_Wheel:
		DEVPOKE2(d->dev_mouse, 0xA, 0, 0);
		break;
	}
	d->exec_state = 0;
residual:
	if (d->running && (d->exec_state || d->queue_count))
		ListPushBack(&emus_needing_work, d, work_link);
	InvalidateUxnScreenRect(d);
	/* If the Uxn CPU is under heavy load, we may end up issuing more invalidations than will be
	 * fulfilled with WM_PAINT events. We could avoid this by issuing less frequently, but that's
	 * not always a clear win -- it makes it buffer and process more mouse moves and redraw less
	 * frequently. In programs like Left that actually makes it less responsive to use input,
	 * though it "completes" more mouse move vector calls.*/
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
		DEVPOKE2(d->dev_mouse, 0xA, x, y);
		*pc = GETVECTOR(d->dev_mouse);
		break;
	case EmuIn_Screen:
		*pc = GETVECTOR(d->dev_screen);
		break;
	case EmuIn_Console:
		d->box->core.dev[0x1].dat[0x2] = bits;
		*pc = GETVECTOR(&d->box->core.dev[0x1]);
		break;
	case EmuIn_Start:
		*pc = UXN_ROM_OFFSET;
		break;
	}
	d->exec_state = type;
#ifndef NDEBUG
	if (type == EmuIn_Start && IsWindowVisible(d->beetbugHWnd)) { PauseVM(d); ShowBeetbugInstruction(d, *pc); return; }
#endif
	RunUxn(d, 0);
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

static HWND CreateUxnWindow(HINSTANCE hInst, LPCSTR file)
{
	RECT rect;
	rect.left = 0; rect.top = 0;
	rect.right = UXN_DEFAULT_WIDTH; rect.bottom = UXN_DEFAULT_HEIGHT;
	AdjustWindowRect(&rect, WS_OVERLAPPEDWINDOW, TRUE);
	return CreateWindowEx(WS_EX_APPWINDOW, EmuWinClass, TEXT("Uxn"), WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, rect.right - rect.left, rect.bottom - rect.top, NULL, NULL, hInst, (void *)file);
}

static void CloneWindow(EmuWindow *a)
{
	HWND hWnd = CreateUxnWindow((HINSTANCE)GetWindowLongPtr(a->hWnd, GWLP_HINSTANCE), NULL);
	ShowWindow(hWnd, SW_SHOW);
	PostMessage(hWnd, UXNMSG_BecomeClone, (WPARAM)a, 0);
}

static int DecodeUxnOpcode(TCHAR *out, BYTE instr)
{
	static LPCSTR const op_names =
	TEXT("LITINCPOPDUPNIPSWPOVRROTEQUNEQGTHLTHJMPJCNJSRSTHLDZSTZLDRSTRLDASTADEIDEOADDSUBMULDIVANDORAEORSFT");
	int n = 3;
	CopyMemory(out, op_names + (instr & 0x1F) * 3, 3 * sizeof(TCHAR));
	if (instr & 0x20) out[n++] = '2';
	if (instr & 0x80) out[n++] = 'k';
	if (instr & 0x40) out[n++] = 'r';
	out[n] = 0;
	return n;
}

enum
{
	BBID_AsmList = 1, BBID_HexList, BBID_WrkStack, BBID_RetStack, BBID_DevMem,
	BBID_Status, BBID_StepBtn, BBID_BigStepBtn, BBID_PauseBtn,
	BBID_PushStackBtn0, BBID_PushStackBtn1, BBID_PopStackBtn0, BBID_PopStackBtn1
};

static LRESULT CALLBACK BeetbugWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	BeetbugWin *d = (BeetbugWin *)GetWindowLongPtr(hWnd, GWLP_USERDATA);
	switch (msg)
	{
	case WM_CREATE:
	{
		LONG_PTR i, j, k; HWND list; LV_COLUMN col; HFONT hFont = GetSmallFixedFont();
		static const int
			columns[] = { /* Instr list */ 45, 25, 50, 0, /* Hex list */ 40, 130, 0,
			              /* Stacks */ 25, 0, 25, 0, /* Device mem */ 20, 130, 0},
			rows[] = {UXN_RAM_SIZE, UXN_RAM_SIZE / 8, 255, 255, 256 / 8},
			status_parts[] = {70, 140, 180, -1};
		d = AllocZeroedOrFail(sizeof *d);
		SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG_PTR)d);
		d->emu = ((CREATESTRUCT *)lParam)->lpCreateParams;
		d->hWnd = hWnd;
		ZeroMemory(&col, sizeof col);
		col.mask = LVCF_FMT | LVCF_WIDTH;
		for (i = j = 0; i < 5; i++)
		{
			(&d->hDisList)[i] = list = CreateWindowEx(
				WS_EX_CLIENTEDGE, WC_LISTVIEW, NULL,
				WS_TABSTOP | WS_CHILD | WS_BORDER | WS_VISIBLE |
					LVS_REPORT | LVS_OWNERDATA | LVS_SHOWSELALWAYS | LVS_NOCOLUMNHEADER | LVS_EDITLABELS | LVS_SINGLESEL,
				0, 0, 0, 0, hWnd, (HMENU)(i + BBID_AsmList), (HINSTANCE)GetWindowLongPtr(hWnd, GWLP_HINSTANCE), NULL);
			if (hFont) SendMessage(list, WM_SETFONT, (WPARAM)hFont, 0);
			ListView_SetExtendedListViewStyle(list, LVS_EX_FULLROWSELECT);
			ListView_DeleteAllItems(list);
			for (k = 0; (col.cx = columns[j++]);) ListView_InsertColumn(list, k++, &col);
			ListView_SetItemCount(list, rows[i]);
			ListView_EnsureVisible(list, 0, FALSE);
		}
		d->hStatus = CreateWindowEx(
			0, STATUSCLASSNAME, NULL, WS_CHILD | WS_VISIBLE | SBARS_SIZEGRIP,
			0, 0, 0, 0, hWnd, (HMENU)BBID_Status, (HINSTANCE)GetWindowLongPtr(hWnd, GWLP_HINSTANCE), NULL);
		SendMessage(d->hStatus, SB_SETPARTS, sizeof status_parts / sizeof(int), (LPARAM)status_parts);
		for (i = 0; i < 7; i++)
		{
			HWND btn = (&d->hBigStepBtn)[i] = CreateWindowEx(0, TEXT("Button"), NULL, WS_TABSTOP | WS_VISIBLE | WS_CHILD, 0, 0, 0, 0, hWnd, (HMENU)(BBID_StepBtn + i), (HINSTANCE)GetWindowLongPtr(hWnd, GWLP_HINSTANCE), NULL);
			SendMessage(btn, WM_SETFONT, i < 3 ? (WPARAM)GetStockObject(DEFAULT_GUI_FONT) : (WPARAM)hFont, 0);
		}
		SetWindowText(d->hBigStepBtn, TEXT("Big Step (F7)"));
		SetWindowText(d->hStepBtn, TEXT("Step (F8)"));
		for (i = 0; i < 4; i++)
			SetWindowText((&d->hStackPushBtn0)[i], i < 2 ? TEXT("PUSH") : TEXT("POP"));
		SetTimer(hWnd, 1, 50, NULL);
		break;
	}
	case WM_CLOSE:
		ShowWindow(hWnd, SW_HIDE);
		return 0;
	case WM_DESTROY:
		HeapFree(GetProcessHeap(), 0, d);
		break;
	case WM_SIZE:
	{
		static const SIZE btnSize = {87, 19}; int i;
		RECT r = {0, 0, LOWORD(lParam), HIWORD(lParam)}, tmp;
		SendMessage(d->hStatus, WM_SIZE, 0, 0);
		GetClientRect(d->hStatus, &tmp);
		MapWindowPoints(d->hStatus, hWnd, (LPPOINT)&tmp, 2); /* must violate strict aliasing */
		r.bottom = tmp.top;
		CutRect(&r, FromLeft, 200, &tmp);
		d->rcBlank = r;
		CutRectForWindow(&tmp, FromBottom, 125, d->hHexList);
		MoveWindowRect(d->hDisList, &tmp, TRUE);
		r.top += 5;
		CutRect(&r, FromTop, btnSize.cy, &tmp);
		r.top += 5;
		tmp.left += 5; tmp.right = tmp.left + 500;
		for (i = 0; i < 3; i++)
		{
			CutRectForWindow(&tmp, FromLeft, btnSize.cx, (&d->hBigStepBtn)[i]);
			tmp.right--;
		}
		for (i = 0; i < 2; i++)
		{
			CutRect(&r, FromLeft, 50, &tmp);
			CutRect(&tmp, FromBottom, 20, &d->rcWstLabel + i);
			CutRectForWindow(&tmp, FromBottom, 15, (&d->hStackPopBtn0)[i]);
			CutRectForWindow(&tmp, FromBottom, 15, (&d->hStackPushBtn0)[i]);
			MoveWindowRect((&d->hWrkStack)[i], &tmp, TRUE);
		}
		CutRect(&r, FromBottom, 20, &d->rcDevMemLabel);
		MoveWindowRect(d->hDevMem, &r, TRUE);
		break;
	}
	case WM_PAINT:
	{
		/* TODO do we actually need this? just use background painting? */
		static const LPCSTR st_labels[] = {TEXT("WST"), TEXT("RST")};
		RECT rTmp; PAINTSTRUCT ps; HDC hDC = BeginPaint(hWnd, &ps);
		TCHAR buff[32], *str; int i, old_bkmode; HGDIOBJ old_font;
		if (IntersectRect(&rTmp, &ps.rcPaint, &d->rcBlank))
			FillRect(hDC, &rTmp, (HBRUSH)(COLOR_3DFACE + 1));
		old_bkmode = SetBkMode(hDC, TRANSPARENT);
		old_font = SelectObject(hDC, GetSmallFixedFont());
		for (i = 0; i < 3; i++)
		{
			RECT *rc = &d->rcWstLabel + i;
			if (!IntersectRect(&rTmp, &ps.rcPaint, rc)) continue;
			if (i < 2)
			{
				wsprintf(buff, "%s %02X", st_labels[i], (UINT)(&d->emu->box->core.wst)[i]->ptr);
				str = buff;
			}
			else str = TEXT("DEVICE MEMORY");
			DrawText(hDC, str, -1, rc, DT_SINGLELINE | DT_CENTER | DT_VCENTER);
		}
		SetBkMode(hDC, old_bkmode);
		SelectObject(hDC, old_font);
		EndPaint(hWnd, &ps);
		break;
	}
	case WM_ACTIVATE:
		if (wParam != WA_INACTIVE) SetFocus(d->hDisList);
		return 0;
	case WM_NOTIFY:
		switch (((LPNMHDR)lParam)->code)
		{
		case LVN_MARQUEEBEGIN: return -1; /* Disable. Broken in Win10 1809 w/o manifest, WTF? */
		case NM_DBLCLK:
			ListView_EditLabel(GetDlgItem(hWnd, wParam), ((NMITEMACTIVATE *)lParam)->iItem);
			return 0;
		case LVN_BEGINLABELEDIT: return !(wParam == BBID_WrkStack || wParam == BBID_RetStack);
		case LVN_ENDLABELEDIT:
		{
			NMLVDISPINFO *inf = (NMLVDISPINFO *)lParam; TCHAR buff[32]; int n;
			if (!inf->item.pszText || (n = lstrlen(inf->item.pszText)) >= 31 - 2) return FALSE;
			buff[0] = '0', buff[1] = 'x'; lstrcpyn(buff + 2, inf->item.pszText, 32 - 2);
			if (!StrToIntEx(buff, STIF_SUPPORT_HEX, &n) || n > 0xFF) return FALSE;
			d->emu->box->core.wst->dat[inf->item.iItem] = (BYTE)n;
			return TRUE;
		}
		case LVN_GETDISPINFO:
		{
			TCHAR buff[1024]; Uxn *core = &d->emu->box->core; LV_DISPINFO *di = (LV_DISPINFO *)lParam;
			UINT iItem = di->item.iItem; UINT addr = iItem;
			if (!(di->item.mask & LVIF_TEXT)) return 0;
			buff[0] = 0;
			switch (wParam)
			{
			case BBID_AsmList: switch (di->item.iSubItem)
				{
				case 0: wsprintf(buff, "%c %04X", core->pc == iItem ? '>' : ' ', (UINT)iItem); break;
				case 1: wsprintf(buff, "%02X", (UINT)core->ram[iItem]); break;
				case 2: DecodeUxnOpcode(buff, (BYTE)core->ram[iItem]); break;
				}
				break;
			case BBID_HexList: addr *= 8; switch (di->item.iSubItem)
			{
				case 0: wsprintf(buff, "%04X", addr); break;
				case 1:
				{
					BYTE *mem = core->ram + addr;
					wsprintf(buff, "%02X%02X %02X%02X %02X%02X %02X%02X",
						(UINT)mem[0], (UINT)mem[1], (UINT)mem[2], (UINT)mem[3],
						(UINT)mem[4], (UINT)mem[5], (UINT)mem[6], (UINT)mem[7]);
					break;
				}
				}
				break;
			case BBID_WrkStack: case BBID_RetStack: addr *= 1;
			{
				Stack *stack = (&core->wst)[wParam - BBID_WrkStack];
				if (addr < stack->ptr) wsprintf(buff, "%02X", (UINT)stack->dat[addr]);
				else buff[0] = ' ', buff[1] = ' ', buff[2] = 0;
				break;
			}
			case BBID_DevMem: addr *= 8; switch (di->item.iSubItem)
			{
			case 0: wsprintf(buff, "%02X", addr); break;
			case 1:
			{
				BYTE *mem = d->emu->box->device_memory + addr;
				wsprintf(buff, "%02X%02X %02X%02X %02X%02X %02X%02X",
					(UINT)mem[0], (UINT)mem[1], (UINT)mem[2], (UINT)mem[3],
					(UINT)mem[4], (UINT)mem[5], (UINT)mem[6], (UINT)mem[7]);
				break;
			}
			}
			}
			lstrcpyn(di->item.pszText, buff, di->item.cchTextMax);
			return 0;
		}
		}
		break;
	case WM_COMMAND:
	{
		int idm = LOWORD(wParam);
		if (lParam && HIWORD(wParam) == BN_CLICKED)
			switch (idm)
			{
			case BBID_StepBtn: idm = IDM_STEP; break;
			case BBID_BigStepBtn: idm = IDM_BIGSTEP; break;
			case BBID_PauseBtn: return SendMessage(d->emu->hWnd, WM_COMMAND, MAKEWPARAM(IDM_PAUSE, 0), 0);

			{
				enum {Flags = LVIS_SELECTED | LVIS_FOCUSED}; int btn, i; HWND hList; Stack *stack;
			case BBID_PushStackBtn0: case BBID_PushStackBtn1: int push = 1; goto push_pop;
			case BBID_PopStackBtn0:  case BBID_PopStackBtn1:      push = 0; push_pop:
				btn = idm - (push ? BBID_PushStackBtn0 : BBID_PopStackBtn0);
				hList = (&d->hWrkStack)[btn]; stack = (&d->emu->box->core.wst)[btn];
				if (stack->ptr == (push ? 255 : 0)) break;
				i = push ? stack->ptr++ : --stack->ptr - 1; if (i < 0) i = 0;
				ListView_SetItemState(hList, i, Flags, Flags);
				SetFocus(hList);
				break;
			}

			}
		if ((idm == IDM_STEP || idm == IDM_BIGSTEP) && !d->emu->running && d->emu->exec_state)
		{
			d->emu->box->core.fault_code = 0;
			RunUxn(d->emu, idm == IDM_STEP ? 1 : 100);
			ShowBeetbugInstruction(d->emu, d->emu->box->core.pc);
			InvalidateRect(d->hDisList, NULL, FALSE);
			return 0;
		}
		break;
	}
	case WM_TIMER:
		if (wParam == 1)
		{
			static const LPCSTR play_texts[] = {0, TEXT("Running"), TEXT("Suspended"), TEXT("Paused")};
			BYTE new_play = d->emu->running ? 1 : d->emu->exec_state ? 2 : 3;
			BOOL step_ctrls = new_play == 2; TCHAR buff[6]; int i;
			// int top = ListView_GetTopIndex(d->hList), bot = top + ListView_GetCountPerPage(d->hList);
			/* TODO ust ListView_RedrawItems() instead? */
			for (i = 0; i < 5; i++) InvalidateRect((&d->hDisList)[i], NULL, FALSE); /* TODO only changed areas */
			for (i = 0; i < 2; i++) InvalidateRect(hWnd, &d->rcWstLabel + i, FALSE);
			if (d->sbar_play_mode != new_play)
			{
				SendMessage(d->hStatus, SB_SETTEXT, 0, (LPARAM)(play_texts[d->sbar_play_mode = new_play]));
				SetWindowText(d->hPauseBtn, new_play == 1 ? TEXT("Pause (F9)") : TEXT("Resume (F9)"));
			}
			if (d->sbar_input_event != d->emu->exec_state)
			{
				static const LPCSTR event_texts[EmuIn_Start + 1] = { /* TODO crappy */
					TEXT(""), TEXT("KeyChar"), TEXT("CtrlDown"), TEXT("CtrlUp"),
					TEXT("CtrlUp"), TEXT("MouseDown"), TEXT("MouseUp"), TEXT("Wheel"),
					TEXT("Screen"), TEXT("ConChar"), TEXT("Init")
				};
				SendMessage(d->hStatus, SB_SETTEXT, 1,
					(LPARAM)event_texts[d->sbar_input_event = d->emu->exec_state]);
			}
			if (d->sbar_pc != d->emu->box->core.pc)
			{
				wsprintf(buff, "%04X", (UINT)(d->sbar_pc = d->emu->box->core.pc));
				SendMessage(d->hStatus, SB_SETTEXT, 2, (LPARAM)buff);
			}
			if (IsWindowEnabled(d->hBigStepBtn) != step_ctrls)
			{
				EnableWindow(d->hBigStepBtn, step_ctrls);
				EnableWindow(d->hStepBtn, step_ctrls);
			}
			return 0;
		}
		break;
	}
	return DefWindowProc(hWnd, msg, wParam, lParam);
}

static LRESULT CALLBACK ConEditWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	LRESULT result;
	if (msg == WM_COMMAND && LOWORD(wParam) == IDM_SELECTALL)
		return SendMessage(hWnd, EM_SETSEL, 0, -1);
	if (msg == WM_KEYDOWN && (wParam == VK_ESCAPE || wParam == VK_RETURN))
		return SendMessage(GetParent(hWnd), msg, wParam, lParam);
	result = CallWindowProc((WNDPROC)GetWindowLongPtr(hWnd, GWLP_USERDATA), hWnd, msg, wParam, lParam);
	if (result && msg == WM_CHAR && GetDlgCtrlID(hWnd) == 1)
	{
		HWND inputHWnd = GetDlgItem(GetParent(hWnd), 2);
		SetFocus(inputHWnd);
		result = SendMessage(inputHWnd, msg, wParam, lParam);
	}
	return result;
}

static LRESULT CALLBACK ConsoleWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	ConWindow *d = (ConWindow *)GetWindowLongPtr(hWnd, GWLP_USERDATA);
	switch (msg)
	{
	case WM_CREATE:
	{
		HWND hwTmp; int i; HFONT hFont = GetSmallFixedFont();
		HINSTANCE hinstTmp = (HINSTANCE)GetWindowLongPtr(hWnd, GWLP_HINSTANCE);
		d = AllocZeroedOrFail(sizeof *d);
		SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG_PTR)d);
		d->outHWnd = CreateWindowEx(
			WS_EX_STATICEDGE, EditWinClass, NULL,
			WS_CHILD | WS_VISIBLE | WS_VSCROLL | ES_MULTILINE | ES_AUTOVSCROLL | ES_READONLY,
			0, 0, 0, 0, hWnd, (HMENU)1, hinstTmp, NULL);
		d->inHWnd = CreateWindowEx(
			WS_EX_CLIENTEDGE, EditWinClass, NULL,
			WS_CHILD | WS_VISIBLE,
			0, 0, 0, 0, hWnd, (HMENU)2, hinstTmp, NULL);
		for (i = 0, hwTmp = d->outHWnd; i < 2; hwTmp = (&d->outHWnd)[++i])
		{
			SetWindowLongPtr(hwTmp, GWLP_USERDATA, (LONG_PTR)GetWindowLongPtr(hwTmp, GWLP_WNDPROC));
			SetWindowLongPtr(hwTmp, GWLP_WNDPROC,  (LONG_PTR)ConEditWndProc);
			if (hFont) SendMessage(hwTmp, WM_SETFONT, (WPARAM)hFont, 0);
		}
		break;
	}
	case WM_CLOSE:
		ShowWindow(hWnd, SW_HIDE);
		return 0;
	case WM_DESTROY:
		HeapFree(GetProcessHeap(), 0, d);
		return 0;
	case WM_SIZE:
        MoveWindow(d->outHWnd, 0, 0, LOWORD(lParam), HIWORD(lParam) - 15, TRUE);
        MoveWindow(d->inHWnd, 0, HIWORD(lParam) - 15, LOWORD(lParam), 15, TRUE);
		return 0;
	case WM_ACTIVATE:
		if (wParam != WA_INACTIVE) SetFocus(d->inHWnd);
		return 0;
	case WM_KEYDOWN:
		if (wParam == VK_ESCAPE) { ShowWindow(hWnd, SW_HIDE); return 0; }
		if (wParam == VK_RETURN)
		{
			TCHAR buff[2048]; HWND parent; EmuWindow *emu;
			LRESULT chars_len = SendMessage(d->inHWnd, WM_GETTEXTLENGTH, 0, 0), i;
			if (chars_len >= 2048 - 1 || !(parent = (HWND)GetWindowLongPtr(hWnd, GWLP_HWNDPARENT))) break;
			emu = (EmuWindow *)GetWindowLongPtr(parent, GWLP_USERDATA);
			chars_len = SendMessage(d->inHWnd, WM_GETTEXT, 2048, (LPARAM)buff);
			SendMessage(d->inHWnd, WM_SETTEXT, 0, (LPARAM)(buff + chars_len));
			buff[chars_len++] = '\n';
			for (i = 0; i < chars_len; i++)
				SendInputEvent(emu, EmuIn_Console, (BYTE)buff[i], 0, 0);
		}
		break;
	case WM_CTLCOLORSTATIC:
		if ((HWND)lParam != d->outHWnd) break;
		return (LRESULT)GetSysColorBrush(COLOR_WINDOW);
	}
	return DefWindowProc(hWnd, msg, wParam, lParam);
}

static LRESULT CALLBACK EmuWndProc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
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
		KillTimer(hwnd, TimerID_Screen60hz);
		SetHostCursorVisible(d, TRUE);
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
		PAINTSTRUCT ps; HDC hDC; BITMAPINFO bmi; RECT crect, tmp;
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
		}
		hDC = BeginPaint(hwnd, &ps);
		if ((UnionRect(&tmp, &ps.rcPaint, &d->viewport_rect), !EqualRect(&d->viewport_rect, &tmp)))
			FillRect(hDC, &ps.rcPaint, (HBRUSH)GetStockObject(BLACK_BRUSH));
		if (!IntersectRect(&tmp, &d->viewport_rect, &ps.rcPaint)) goto done_painting;
		if (!d->hBMP || !d->hDibDC) goto done_painting;
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
		/* One-line version, doesn't need the retained stuff, but probably slower: */
		/* SetDIBitsToDevice(hDC, 0, 0, UXN_DEFAULT_WIDTH, UXN_DEFAULT_HEIGHT, 0, 0, 0, UXN_DEFAULT_HEIGHT, uxn_screen.pixels, &bmi, DIB_RGB_COLORS); */
		/* SetDIBits(d->hDibDC, d->hBMP, 0, UXN_DEFAULT_HEIGHT, d->screen.pixels, &bmi, DIB_RGB_COLORS); */
		SelectObject(d->hDibDC, d->hBMP);
		if (d->viewport_scale == 1)
			BitBlt(hDC, d->viewport_rect.left, d->viewport_rect.top, d->screen.width, d->screen.height, d->hDibDC, 0, 0, SRCCOPY);
		else
			StretchBlt(hDC, d->viewport_rect.left, d->viewport_rect.top, d->viewport_rect.right - d->viewport_rect.left, d->viewport_rect.bottom - d->viewport_rect.top, d->hDibDC, 0, 0, d->screen.width, d->screen.height, SRCCOPY);
	done_painting:
		EndPaint(hwnd, &ps);
		d->last_paint = TimeStampNow();
		if (d->needs_audio == 1)
		{
			d->needs_audio = 2;
			SetTimer(d->hWnd, TimerID_InitAudio, 100, NULL);
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
		InvalidateRect(d->hWnd, NULL, FALSE);
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
		mouse_in_uxn = PtInRect(&d->viewport_rect, mouse) && d->running && GETVECTOR(d->dev_mouse);
		/* TODO Vector check is slightly wrong -- it doesn't, but should, check when the mouse vector has been changed in uxn code without the mouse moving. Test repro: launch something with launcher.rom and don't move the mouse. (If you clicked instead of using keyboard, don't release the click button.) */
		SetHostCursorVisible(d, !mouse_in_uxn);
		if (!mouse_in_uxn) break;
		BindPointToLocalUxnScreen(&d->viewport_rect, d->viewport_scale, &mouse);
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
	case WM_SYSCHAR: /* Holding alt and pressing a key */
		if (d->running && !d->host_cursor) goto char_down; /* Use alt+letter as key input when VM is focused and running */
		break;
	case WM_KEYDOWN: case WM_SYSKEYDOWN: case WM_KEYUP: case WM_SYSKEYUP:
	{
		int up = (int)lparam & 1 << 31, was_down = lparam & 1 << 30, bits = 0;
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
	other_vkey: /* Holding control and pressing a keyboard key */
		if (!(GetKeyState(VK_CONTROL) & 0x8000)) break; /* If ctrl isn't held, let it be handled as WM_CHAR instead. */
		if (up || !(wparam = MapVirtualKey(wparam, MAPVK_VK_TO_CHAR))) break;
		/* Holding Alt or Ctrl causes characters to appear upper case, so if shift isn't held, turn 'em lower. */
		if (wparam >= 'A' && wparam <= 'Z' && !(!(GetKeyState(VK_SHIFT) & 0x8000) ^ !(GetKeyState(VK_CAPITAL)))) wparam += 0x20;
	case WM_CHAR: char_down: /* WM_CHAR is pressing a key without holding alt or ctrl */
		/* Disallow control characters except tab, newline, etc. */
		if (wparam < 32 && wparam != 8 && wparam != 9 && wparam != 10 && wparam != 13 && wparam != 27) break;
		SendInputEvent(d, EmuIn_KeyChar, (BYTE)wparam, 0, 0);
		return 0;
	}
	case WM_NCMOUSEMOVE:
	case WM_MOUSELEAVE:
		SetHostCursorVisible(d, TRUE);
		break;
	case WM_TIMER:
		switch (wparam)
		{
		case TimerID_Screen60hz:
			SendInputEvent(d, EmuIn_Screen, 0, 0, 0);
			return 0;
		case TimerID_InitAudio:
			KillTimer(hwnd, TimerID_InitAudio);
			/* In Windows XP VM, debugger attached with audio playing can cause program to freeze.
			 * Should probably modify this or remove it once we have a muting option. */
#if !defined(NDEBUG) && WINVER < 0x0500
			if (!IsDebuggerPresent())
#endif
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
		case IDM_STEP: case IDM_BIGSTEP:
			if (!d->running && d->exec_state)
			{
				OpenBeetbugWindow(d);
				SetFocus(d->beetbugHWnd);
				SendMessage(d->beetbugHWnd, msg, wparam, lparam);
			}
			return 0;
		case IDM_TOGGLECONSOLE:
			if (!d->consoleHWnd) CreateConsoleWindow(d);
			ShowWindow(d->consoleHWnd, IsWindowVisible(d->consoleHWnd) ? SW_HIDE : SW_SHOW);
			return 0;
		case IDM_OPENBEETBUG:
			OpenBeetbugWindow(d);
			return 0;
		}
		break;

	case UXNMSG_ContinueExec:
		if (!d->running) return 0;
		if (d->exec_state) RunUxn(d, 0); /* Unfinished vector execution */
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
		if (b->running) UnpauseVM(d);
		UpdateWindow(d->hWnd);
		return 0;
	}
	}
	return DefWindowProc(hwnd, msg, wparam, lparam);
}

int CALLBACK WinMain(HINSTANCE instance, HINSTANCE prev_instance, LPSTR command_line, int show_code)
{
	WNDCLASSEX wc; HWND hWin, hParent;
	MSG msg; HACCEL hAccel;
	(void)command_line; (void)prev_instance;
	QueryPerformanceFrequency(&_perfcount_freq);
	ExecutionTimeLimit = _perfcount_freq.QuadPart / 20;
	ZeroMemory(&wc, sizeof wc);
	wc.hInstance = instance;
	wc.cbSize = sizeof wc;
	wc.lpfnWndProc = EmuWndProc;
	wc.lpszClassName = EmuWinClass;
	wc.lpszMenuName = MAKEINTRESOURCE(IDC_UXN32);
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hIcon = LoadIcon(instance, (LPCTSTR)IDI_UXN32); /* use this one */
	wc.style = CS_HREDRAW | CS_VREDRAW;
	RegisterClassEx(&wc);
	wc.lpszClassName = ConsoleWinClass;
	wc.lpszMenuName = NULL;
	wc.lpfnWndProc = ConsoleWndProc;
	RegisterClassEx(&wc);
	wc.lpszClassName = BeetbugWinClass;
	wc.lpfnWndProc = BeetbugWndProc;
	RegisterClassEx(&wc);
	hAccel = LoadAccelerators(instance, (LPCSTR)IDC_UXN32);
	InitCommonControls();

	hWin = CreateUxnWindow(instance, TEXT("launcher.rom"));
	ShowWindow(hWin, show_code);

	for (;;)
	{
		while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
		{
			if (msg.message == WM_QUIT) goto quitting;
			if (msg.message == WM_KEYDOWN || msg.message == WM_SYSKEYDOWN)
			{
				hWin = msg.hwnd;
				if (msg.wParam >= VK_F1 && msg.wParam <= VK_F24)
					while ((hParent = (HWND)GetWindowLongPtr(hWin, GWLP_HWNDPARENT))) hWin = hParent;
				if (TranslateAccelerator(hWin, hAccel, &msg)) continue;
			}
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
