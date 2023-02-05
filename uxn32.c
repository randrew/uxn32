#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include "resource.h"
#include "uxn_core.h"
#include <windows.h>
#include <windowsx.h>
#include <shellapi.h>
#include <shlwapi.h>
#include <commdlg.h>
#include <commctrl.h>
#include <mmsystem.h>

#pragma warning(disable:4244) /* Noisy VC6 warning. Can't disable with flag */
#if !defined(GWLP_WNDPROC)
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

#define HOST_PAGE_SIZE 4096

#define UXN_DEFAULT_WIDTH (64 * 8)
#define UXN_DEFAULT_HEIGHT (40 * 8)
#define UXN_RAM_SIZE 0x10000u
/* ^ Memory normally usable to the Uxn VM and programs. */
#define UXN_RAM_PAD_SIZE 0x1u
/* ^ Extra byte of RAM that stops a Uxn 16-bit POKE/PEEK op at 0xFFFF doing evil things. */
#define UXN_ROM_OFFSET 0x0100u
/* ^ Start location in Uxn RAM where a loaded 'ROM' file is copied to. */
#define UXN_SAMPLE_RATE 44100
#define UXN_VOICES 4

#define UXN_FAULT_STASHER 253
#define UXN_FAULT_DEBUG 254
#define UXN_FAULT_QUIT 255

#define VV_SYSTEM	0x00
#define VV_CONSOLE	0x10
#define VV_SCREEN	0x20
#define VV_AUDIO0	0x30
#define VV_AUDIO1	0x40
#define VV_AUDIO2	0x50
#define VV_AUDIO3	0x60
#define VV_CONTROL	0x80
#define VV_MOUSE	0x90
#define VV_FILE0	0xA0
#define VV_FILE1	0xB0
#define VV_DATE	0xC0

#define DEVPEEK(d, o, x) ((o) = ((d)[(x)] << 8) + (d)[(x) + 1])
#define DEVPOKE(d, a, v) ((d)[(a)] = (v) >> 8, (d)[(a) + 1] = (v))
#define DEVPEEK2(d, o1, o2, a) (DEVPEEK(d, o1, a), DEVPEEK(d, o2, (a) + 2))
#define DEVPOKE2(d, a, v1, v2) (DEVPOKE(d, a, v1), DEVPOKE(d, (a) + 2, v2))
#define GET_16BIT(d) ((d)[0] << 8 | (d)[1])
#define DEVINDEX(device, base) (((device) - (base)) >> 4)

typedef LPWSTR * WINAPI Type_CommandLineToArgvW(LPCWSTR lpCmdLine, int* pNumArgs);
typedef LPWSTR WINAPI Type_GetCommandLineW(void);

#define ADAPTER_VBLANK 0
/* ^ If enabled, time the Uxn "screen" device vector using D3DKMTWaitForVerticalBlankEvent. */
#if ADAPTER_VBLANK
typedef UINT D3DDDI_VIDEO_PRESENT_SOURCE_ID;
typedef UINT D3DKMT_HANDLE;
typedef struct _D3DKMT_OPENADAPTERFROMHDC
{
    HDC hDc;
    D3DKMT_HANDLE hAdapter;
    LUID AdapterLuid;
    D3DDDI_VIDEO_PRESENT_SOURCE_ID VidPnSourceId;
} D3DKMT_OPENADAPTERFROMHDC;
typedef struct _D3DKMT_WAITFORVERTICALBLANKEVENT
{
    D3DKMT_HANDLE hAdapter;
    D3DKMT_HANDLE hDevice;
    D3DDDI_VIDEO_PRESENT_SOURCE_ID VidPnSourceId;
} D3DKMT_WAITFORVERTICALBLANKEVENT;
typedef BOOL WINAPI Type_EnumDisplayDevicesA(LPCSTR lpDevice, DWORD iDevNum, PDISPLAY_DEVICEA lpDisplayDevice, DWORD dwFlags);
static Type_EnumDisplayDevicesA *Ptr_EnumDisplayDevicesA;
typedef LONG WINAPI Type_D3DKMTOpenAdapterFromHdc(D3DKMT_OPENADAPTERFROMHDC* in_out);
static Type_D3DKMTOpenAdapterFromHdc *Ptr_D3DKMTOpenAdapterFromHdc;
typedef LONG WINAPI Type_D3DKMTWaitForVerticalBlankEvent(CONST D3DKMT_WAITFORVERTICALBLANKEVENT *in);
static Type_D3DKMTWaitForVerticalBlankEvent *Ptr_D3DKMTWaitForVerticalBlankEvent;
#endif

static LARGE_INTEGER _perfcount_freq;
static LONGLONG ExecutionTimeLimit;
#define RepaintTimeLimit ExecutionTimeLimit
static HINSTANCE MainInstance;
static LPCSTR EmuWinClass = TEXT("uxn_emu_win"), ConsoleWinClass = TEXT("uxn_console_win"), EditWinClass = TEXT("EDIT");
static LPCSTR BeetbugWinClass = TEXT("uxn_beetbug_win");
static LPCSTR DefaultROMPath = TEXT("launcher.rom");
static LPWSTR *CmdLineArgs; static int CmdLineArgCount;
static BOOL BreakOnInitVector;
static HANDLE VBlankMutex, ResumeTimerEvent;
static HANDLE ProcessHeap;

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
static void * _impl_ListFront(LinkedList *list, SIZE_T offset)
{ return list->front ? (char *)list->front - offset : NULL; }
static void * _impl_ListNext(ListLink *node, SIZE_T offset)
{ return node->next ? (char *)node->next - offset : NULL; }
/* ^ Could move (a)->link_field to here instead of in macro using offset. */

#define ListPopFront(list, type, link_field) OUTER_OF(_impl_ListPopFront(list), type, link_field)
#define ListPushBack(list, a, link_field) _impl_ListPushBack((list), &(a)->link_field)
#define ListRemove(list, a, link_field) _impl_ListRemove((list), &(a->link_field))
#define ListLinkUsed(list, a, link_field) ((a)->link_field.prev || (list)->front == &(a)->link_field)
#define ListFront(list, type, link_field) ((type *)_impl_ListFront(list, OFFSET_OF(type, link_field)))
#define ListNext(a, type, link_field) ((type *)_impl_ListNext(&(a)->link_field, OFFSET_OF(type, link_field)))

/* From uxn_lz.c. They return -1 on failure.*/
int uxn_lz_compress(void *output, int output_size, const void *input, int input_size);
int uxn_lz_decompress(void *output, int output_size, const void *input, int input_size);

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

typedef struct UxnStashFooter
{
	BYTE secret_byte[UXN_RAM_PAD_SIZE]; /* Uxn VM can read and write this byte (1 byte after 64k main RAM) */
	USHORT slot;
	ListLink link;
} UxnStashFooter;
/* The footer is put after the 64k of RAM. Currently using HeapAlloc, but a pool allocator would be better. */
typedef BYTE *UxnStashPtr;
enum { StashMetadataHostPages = ((USHORT)-1 + 1) * sizeof(UxnStashPtr) / HOST_PAGE_SIZE};
#define STASH_RAMToMeta(ram) ((UxnStashFooter *)(ram + UXN_RAM_SIZE))
#define STASH_MetaToRAM(footer) ((BYTE *)footer - UXN_RAM_SIZE)
typedef struct UxnBox /* CPU + memory only */
{
	UxnCore core;
	UxnStack work_stack, ret_stack;
	UxnU8 device_memory[256];
	UxnStashPtr *table;
	UINT commit_mask[StashMetadataHostPages / 8 / sizeof(UINT)];
	LinkedList stashes;
} UxnBox;
typedef struct UxnVoice
{
	ULONG count, advance, period, age, a, d, s, r;
	UxnU16 wave_base, i, len;
	UxnI8 volume[2];
	UxnU8 repeat;
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
	ULONG palette[4];
	LONG width, height;
	UxnU8 *bg, *fg;
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
typedef struct UxnDebugSymbols
{
	UINT count;
	USHORT *addresses;
	CHAR **strings;
	void *buffer;
} UxnDebugSymbols;

enum { TimerID_InitAudio = 1, TimerID_FlushConsole };
enum { UXNMSG_ContinueExec = WM_USER, UXNMSG_BecomeClone, UXNMSG_LoadSymbols, UXNMSG_SendArgs, UXNMSG_Screen60hz };
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
	EmuIn_DebugJump,
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
	UxnBox box;
	HWND hWnd, consoleHWnd, beetbugHWnd;
	HMENU hHiddenMenu;
	HBITMAP hBMP;
	HDC hDibDC;
	SIZE dib_dims;

	BYTE running, exec_state, last_event, host_cursor;

	EmuInEvent *queue_buffer;
	USHORT queue_count, queue_first;
	ListLink work_link, vblank_link;
	LONGLONG last_paint, instr_count;

	RECT viewport_rect;
	LONG viewport_scale;
	UxnScreen screen;
	UxnFiler filers[2];
	UxnVoice synth_voices[UXN_VOICES];
	UxnWaveOut *wave_out;

	BYTE needs_audio; /* 3 states, 0: not needed, 1: plz init, 2: init started */
	TCHAR rom_path[MAX_PATH];
} EmuWindow;
typedef struct ConWindow
{
	HWND outHWnd, inHWnd;
	DWORD count;
	CHAR buffer[256];
	BYTE has_newline;
} ConWindow;
enum
{
	BB_AsmList = 0, BB_HexList, BB_WrkStack, BB_RetStack, BB_DevMem,
	BB_Status, BB_BigStepBtn, BB_StepBtn, BB_PauseBtn, BB_JumpBtn,
	BB_PushStackBtn0, BB_PushStackBtn1, BB_PopStackBtn0, BB_PopStackBtn1,
	BB_JumpEdit,
	BB_MAX
};
typedef struct BeetbugWin
{
	EmuWindow *emu;
	HWND ctrls[BB_MAX];
	USHORT sbar_pc, sbar_fault, sbar_flashing;
	LONGLONG sbar_instrcount;
	RECT rcBlank, rcWstLabel, rcRstLabel, rcDevMemLabel;
	UxnDebugSymbols symbols;
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
#ifndef NDEBUG
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

static void * HeapAlloc0(SIZE_T bytes)
{ return HeapAlloc(ProcessHeap, HEAP_ZERO_MEMORY, bytes); }
static BOOL HeapFree0(void * mem)
{ return HeapFree(ProcessHeap, 0, mem); }
static void * HeapAlloc0OrDie(SIZE_T bytes)
{
	void *result = HeapAlloc(ProcessHeap, HEAP_ZERO_MEMORY, bytes);
	if (!result) OutOfMemory();
	return result;
}

static void ResetStasher(UxnBox *box)
{
	UxnStashFooter *s, *n;
	for (s = ListFront(&box->stashes, UxnStashFooter, link); s; s = n)
		n = ListNext(s, UxnStashFooter, link), HeapFree0(STASH_MetaToRAM(s));
	ZeroMemory(&box->stashes, sizeof box->stashes);
	ZeroMemory(box->commit_mask, sizeof box->commit_mask);
	VirtualFree(box->table, sizeof(UxnStashPtr) * (USHORT)-1, MEM_DECOMMIT);
}
static BYTE * GetStashMemory(UxnBox *box, USHORT slot)
{
	BYTE *memory;
#if 0 /* If UxnStashPtr does not evenly divide HOST_PAGE_SIZE */
	UINT a =   slot    * sizeof(UxnStashPtr)      / HOST_PAGE_SIZE,
	     b = ((slot+1) * sizeof(UxnStashPtr) - 1) / HOST_PAGE_SIZE;
	for (; a <= b; a++)
	{
		UINT index = a >> 5, bit = 1 << (a & 31), piece;
		if ((piece = box->commit_mask[index]) & bit) continue;
		box->commit_mask[index] = piece | bit;
		VirtualAlloc((BYTE *)box->table + a * HOST_PAGE_SIZE, 1, MEM_COMMIT, PAGE_READWRITE);
	}
#else /* If UxnStashPtr evenly divides HOST_PAGE_SIZE */
	UINT a = slot * sizeof(UxnStashPtr) / HOST_PAGE_SIZE, index = a >> 5, bit = 1 << (a & 31), piece;
	if (!((piece = box->commit_mask[index]) & bit))
	{
		box->commit_mask[index] = piece | bit;
		VirtualAlloc((BYTE *)box->table + a * HOST_PAGE_SIZE, 1, MEM_COMMIT, PAGE_READWRITE);
	}
#endif
	if (!(memory = box->table[slot]))
	{
		UxnStashFooter *s;
		box->table[slot] = memory = HeapAlloc0(UXN_RAM_SIZE + sizeof(UxnStashFooter));
		if (memory) s = STASH_RAMToMeta(memory), s->slot = slot, ListPushBack(&box->stashes, s, link);
	}
	return memory;
}

static void CopyStasher(UxnBox *dst, UxnBox *src)
{
	UxnStashFooter *s;
	ResetStasher(dst);
	for (s = ListFront(&src->stashes, UxnStashFooter, link); s; s = ListNext(s, UxnStashFooter, link))
		CopyMemory(GetStashMemory(dst, s->slot), STASH_MetaToRAM(s), UXN_RAM_SIZE + UXN_RAM_PAD_SIZE);
	dst->core.ram = GetStashMemory(dst, 0);
}

static BOOL LoadFileInto(LPCSTR path, BYTE *dest, DWORD max_bytes, DWORD *bytes_read)
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

static const UxnU8 SpriteBlendingTable[5][16] = {
	{0, 0, 0, 0, 1, 0, 1, 1, 2, 2, 0, 2, 3, 3, 3, 0},
	{0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3},
	{1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1},
	{2, 3, 1, 2, 2, 3, 1, 2, 2, 3, 1, 2, 2, 3, 1, 2},
	{1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0}};

static void DrawUxnSprite(UxnScreen *p, UxnU8 *layer_pixels, UxnU16 x, UxnU16 y, UxnU8 *sprite, int color, int flipx, int flipy, int twobpp)
{
	int v, h, opaque = SpriteBlendingTable[4][color], width = p->width, height = p->height;
	int c, ch;
	for (v = 0; v < 8; v++)
	{
		c = sprite[v] | (sprite[v + 8] * twobpp) << 8;
		for (h = 7; h >= 0; --h, c >>= 1)
		{
			ch = (c & 1) | ((c >> 7) & 2);
			if (opaque || ch)
			{
				/* TODO ok this has gotten pretty bloated... we should probably split this up into specialized subroutines for the different combination of options to speed it up. */
				int x0 = (x + (flipx ? 7 - h : h)) & 0xFFFF, y0 = (y + (flipy ? 7 - v : v)) & 0xFFFF;
				if (x0 < width && y0 < height)
					layer_pixels[x0 + y0 * width] = SpriteBlendingTable[ch][color];
			}
		}
	}
}

static void SetUxnScreenSize(UxnScreen *p, DWORD width, DWORD height)
{
	DWORD one_size = width * height, two_size = one_size * 2;
	BOOL reallocing = p->bg != NULL;
	/* TODO pad for simd */
	UxnU8 *buff = reallocing ? HeapReAlloc(ProcessHeap, 0, p->bg, two_size) : HeapAlloc(ProcessHeap, HEAP_ZERO_MEMORY, two_size);
	if (!buff) OutOfMemory();
	if (reallocing) ZeroMemory(buff, two_size);
	p->bg = buff;
	p->fg = buff + one_size;
	p->width = width;
	p->height = height;
}

static void FreeUxnScreen(UxnScreen *p)
{
	if (p->bg) HeapFree0(p->bg);
}

static void RefitEmuWindow(EmuWindow *d)
{
	RECT c, r;
	c.left = c.top = 0;
	c.right = d->screen.width * d->viewport_scale; c.bottom = d->screen.height * d->viewport_scale;
	AdjustWindowRect(&c, GetWindowLong(d->hWnd, GWL_STYLE), !d->hHiddenMenu); /* note: no spam protection from Uxn program yet */
	GetWindowRect(d->hWnd, &r);
	MoveWindow(d->hWnd, r.left, r.top, c.right - c.left, c.bottom - c.top, TRUE);
	/* Seems like the repaint from this is always async. We need the TRUE flag for repainting or the non-client area will be messed up on non-DWM. */
}

static void CalcUxnViewport(EmuWindow *d)
{
	RECT crect, *vprect = &d->viewport_rect; LONG width, height, scale;
	GetClientRect(d->hWnd, &crect);
	width = crect.right / d->screen.width, height = crect.bottom / d->screen.height;
	scale = width < height ? width : height;
	if (scale < 1) scale = 1;
	d->viewport_scale = scale;
	width = scale * d->screen.width, height = scale * d->screen.height;
	vprect->left = (crect.right - width) / 2, vprect->top = (crect.bottom - height) / 2;
	vprect->right = vprect->left + width; vprect->bottom = vprect->top + height;
}

static void ResetFiler(UxnFiler *f)
{
	if (f->hFile != INVALID_HANDLE_VALUE) { CloseHandle(f->hFile); f->hFile = INVALID_HANDLE_VALUE; }
	if (f->hFind != INVALID_HANDLE_VALUE) { FindClose(f->hFind); f->hFind = INVALID_HANDLE_VALUE; }
	f->state = FileDevState_Init;
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

static BOOL PathExtensionBanned(CHAR *path) /* Path length must be <= than MAX_PATH */
{
	static CHAR const *outlawed[] = {"exe", "com", "dll", 0};
	CHAR const **c = outlawed, *ext = PathFindExtensionA(path);
	if (!*ext++) return FALSE;
	while (*c) if (lstrcmpiA(ext, *c++) == 0) return TRUE;
	return FALSE;
}

/* TODO now that there's two filers, I'm especially not happy with the arguments and stuff for these file functions. */
static void FileDevPathChange(EmuWindow *emu, UINT device, UxnFiler *f)
{
	DWORD addr, i, avail;
	char tmp[MAX_PATH + 1], *in_mem;
	ResetFiler(f);
	DEVPEEK(emu->box.device_memory + device, addr, 0x8);
	avail = UXN_RAM_SIZE - addr;
	in_mem = (char *)GetStashMemory(&emu->box, 0) + addr;
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
	if (!PathIsPrefixA(tmp, f->path) || PathExtensionBanned(f->path)) goto error;
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
			if (find_data->cFileName[0] == '.' && /* Skip '.' and '..' */
				(find_data->cFileName[1] == 0 || (find_data->cFileName[1] == '.' && find_data->cFileName[2] == 0))) goto next;
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
	f->state = FileDevState_Writing;
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
	/* result = PathIsDirectoryA(f->path) ? RemoveDirectoryA(f->path) : DeleteFileA(f->path); */
	/* ^ If Uxn is ever allowed to create directories, this is how it would delete them.
	 * But right now, unable to create them, it's just a liability when running in Wine. */
	result = PathIsDirectoryA(f->path) ? 0 : DeleteFileA(f->path);
	return result ? 0 : 1;
}

#define NOTE_PERIOD (UXN_SAMPLE_RATE * 0x4000 / 11025)
#define ADSR_STEP (UXN_SAMPLE_RATE / 0xF)

static const ULONG advances[12] = {
	0x80000, 0x879C8, 0x8FACD, 0x9837F, 0xA1451, 0xAADC1,
	0xB504F, 0xBFC88, 0xCB2FF, 0xD7450, 0xE411F, 0xF1A1C
};

static INT VoiceEnvelope(UxnVoice *c, ULONG age)
{
	if (!c->r)      return 0x0888;
	if (age < c->a) return 0x0888 * age / c->a;
	if (age < c->d) return 0x0444 * (2 * c->d - c->a - age) / (c->d - c->a);
	if (age < c->s) return 0x0444;
	if (age < c->r) return 0x0444 * (c->r - age) / (c->r - c->s);
	c->advance = 0;
	return 0x0000;
}

static int VoiceRender(UxnVoice *c, UxnU8 *uxn_ram, SHORT *out, SHORT *end)
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
		s = (UxnI8)(uxn_ram[(c->wave_base + c->i) % UXN_RAM_SIZE] + 0x80) * VoiceEnvelope(c, c->age++);
		*out++ += s * c->volume[0] / (0x180 * 2); /* Original: / (0x180 * 1) */
		*out++ += s * c->volume[1] / (0x180 * 2); /* Temporarily make this quieter until we add volume slider */
	}
	/* if (!c->advance) audio_is_finished(c); */
	return 1;
}

static void VoiceStart(UxnVoice *c, UxnU16 adsr, UxnU8 pitch)
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

static UxnU8 VoiceCalcVU(UxnVoice *c)
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
	BYTE *ram = GetStashMemory(&d->box, 0);
	int i, still_running; MMRESULT res; (void)res;
	if (!wave_out->hWaveOut) return;
	wave_out->which_buffer = 1 - wave_out->which_buffer;
	ZeroMemory(hdr, sizeof(WAVEHDR));
	hdr->dwBufferLength = AUDIO_BUF_SAMPLES * 2 * sizeof(SHORT);
	hdr->lpData = (LPSTR)samples;
	ZeroMemory(samples, AUDIO_BUF_SAMPLES * 2 * sizeof(SHORT));
	for (i = still_running = 0; i < UXN_VOICES; i++)
		still_running |= VoiceRender(&d->synth_voices[i], ram, samples, samples + AUDIO_BUF_SAMPLES * 2);
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
	d->wave_out = HeapAlloc0OrDie(sizeof(UxnWaveOut));
	d->wave_out->sampleBuffers[0] = HeapAlloc0OrDie(AUDIO_BUF_SAMPLES * 2 * sizeof(SHORT) * 2);
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

static void DevOut_Audio(EmuWindow *emu, UINT device, UINT port)
{
	UxnU8 *imem = emu->box.device_memory + device;
	UxnVoice *voice = &emu->synth_voices[DEVINDEX(device, VV_AUDIO0)];
	UxnU16 adsr;
	if (port != 0xF) return;
	DEVPEEK(imem, adsr, 0x8);
	DEVPEEK(imem, voice->len, 0xA);
	DEVPEEK(imem, voice->wave_base, 0xC);
	voice->volume[0] = imem[0xE] >> 4;
	voice->volume[1] = imem[0xE] & 0xF;
	voice->repeat = !(imem[0xF] & 0x80);
	VoiceStart(voice, adsr, imem[0xF] & 0x7F);
	/* Defer initializing audio until after at least one paint event, because the window might not be visible yet, and this can cause a 50ms+ freeze, increasing the delay before the window is shown. */
	if (!emu->needs_audio) emu->needs_audio = 1;
}

static void DevOut_File(EmuWindow *emu, UINT device, UINT port)
{
	DWORD result = 0, /* next inits suppress msvc warning */ out_len = 0; char *out = 0;
	UxnU8 *imem = emu->box.device_memory + device;
	UxnFiler *f = &emu->filers[DEVINDEX(device, VV_FILE0)];
	switch (port) /* These need write location and size */
	{
	int peek_at; DWORD dst, avail;
	case 0x5: peek_at = 0x4; goto calc;
	case 0xD: peek_at = 0xC; goto calc;
	case 0xF: peek_at = 0xE; goto calc;
	calc:
		DEVPEEK(imem, dst, peek_at);
		DEVPEEK(imem, out_len, 0xA);
		avail = UXN_RAM_SIZE - dst;
		if (out_len > avail) out_len = avail;
		out = (char *)GetStashMemory(&emu->box, 0) + dst;
	}
	switch (port)
	{
	case 0x5: result = FileDevStat(f, out, out_len); goto result;
	case 0x6: result = FileDevDelete(f); goto result;
	case 0x9: result = 0; FileDevPathChange(emu, device, f); goto result;
	case 0xD: result = FileDevRead(f, out, out_len); goto result;
	case 0xF: result = FileDevWrite(f, out, out_len, imem[0x7]); goto result;
	}
	return;
result:
	DEVPOKE(imem, 0x2, result);
}

static void CreateConsoleWindow(EmuWindow *emu)
{
	DWORD exStyle = WS_EX_TOOLWINDOW, wStyle = WS_SIZEBOX | WS_SYSMENU;
	RECT rect; rect.left = 0; rect.top = 0; rect.right = 400; rect.bottom = 200;
	AdjustWindowRectEx(&rect, wStyle, FALSE, exStyle);
	emu->consoleHWnd = CreateWindowEx(exStyle, ConsoleWinClass, TEXT("Console"), wStyle, CW_USEDEFAULT, CW_USEDEFAULT, rect.right - rect.left, rect.bottom - rect.top, emu->hWnd, NULL, MainInstance, (void *)NULL);
}

static void FlushUxnConsole(ConWindow *con, HWND hWnd)
{
	int len = GetWindowTextLength(con->outHWnd);
	KillTimer(hWnd, TimerID_FlushConsole);
	con->buffer[con->count] = 0;
	SendMessage(con->outHWnd, EM_SETSEL, len, len);
	SendMessage(con->outHWnd, EM_REPLACESEL, 0, (LPARAM)con->buffer);
	con->count = 0;
}

/* TODO handle different types of failures */
static BOOL LoadROMIntoBox(UxnBox *box, LPCSTR filename)
{
	DWORD skip, to_read, bytes_read, i; BOOL result = FALSE;
	HANDLE hFile = CreateFile(filename, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hFile == INVALID_HANDLE_VALUE) goto file_error;
	for (i = 0;; i++)
	{
		/* Stash 0 loads at 0x100, stash 1 and later load at 0x0 */
		skip = i ? 0 : UXN_ROM_OFFSET, to_read = UXN_RAM_SIZE - skip;
		if (!ReadFile(hFile, GetStashMemory(box, (USHORT)i) + skip, to_read, &bytes_read, NULL)) goto file_error;
		if (bytes_read < to_read) break;
	}
	result = TRUE;
done:
	if (hFile != INVALID_HANDLE_VALUE) CloseHandle(hFile);
	return result;
file_error:
#if 0
	TCHAR tmp[MAX_PATH]; DWORD res = GetFullPathNameA(filename, MAX_PATH, tmp, NULL);
	if (res == 0 || res >= MAX_PATH) tmp[0] = 0;
	FmtBox(0, "ROM File Load Error", MB_OK | MB_ICONWARNING, "Tried and failed to load the ROM file:\n\n%s\n\nDoes it exist?", tmp);
#endif
	goto done;
}

static UxnU8 UxnDeviceRead(UxnCore *u, UINT address)
{
	UxnBox *box = OUTER_OF(u, UxnBox, core);
	EmuWindow *emu = OUTER_OF(box, EmuWindow, box);
	UINT device = address & 0xF0, port = address & 0x0F;
	UxnU8 *imem = box->device_memory + device;

	switch (address)
	{
	case VV_SYSTEM|0x2: return box->work_stack.num;
	case VV_SYSTEM|0x3: return box->ret_stack.num;

	case VV_SCREEN|0x2: return emu->screen.width >> 8;
	case VV_SCREEN|0x3: return emu->screen.width;
	case VV_SCREEN|0x4: return emu->screen.height >> 8;
	case VV_SCREEN|0x5: return emu->screen.height;
	}

	switch (device)
	{
	case VV_AUDIO0: case VV_AUDIO1: case VV_AUDIO2: case VV_AUDIO3:
	{
		UxnVoice *voice = &emu->synth_voices[DEVINDEX(device, VV_AUDIO0)];
		if (!emu->wave_out || !emu->wave_out->hWaveOut) break;
		switch (port)
		{
		case 0x4: return VoiceCalcVU(voice);
		case 0x2: DEVPOKE(imem, 0x2, voice->i); break; /* TODO uhh this is weird. */
		}
		break;
	}
	case VV_FILE0: case VV_FILE1:
	{
		DWORD result = 0; UxnFiler *f = &emu->filers[DEVINDEX(device, VV_FILE0)];
		switch (port)
		{
		case 0xC: case 0xD:
			result = FileDevRead(f, (char *)&box->device_memory[address], 1);
			DEVPOKE(imem, 0x2, result);
		}
		break;
	}
	case VV_DATE:
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
	}
	}
	return box->device_memory[address];
}

static void UxnDeviceWrite_Cold(UxnBox *box, UINT address, UINT value)
{
	EmuWindow *emu = OUTER_OF(box, EmuWindow, box);
	UINT device = address & 0xF0, port = address & 0x0F;
	UxnU8 *imem = box->device_memory + device;
	if (address == VV_SCREEN + 0x5)
	{
		DWORD w, h;
		DEVPEEK2(imem, w, h, 0x2);
		if (w > 1024 || h > 1024)
		{
			/* If the size is unacceptable, write back the old one */
			DEVPOKE2(imem, 0x2, emu->screen.width, emu->screen.height);
		}
		else
		{
			SetUxnScreenSize(&emu->screen, w, h);
			if (IsZoomed(emu->hWnd)) /* When maximized, adjust the viewport, not the window size. */
			{
				RECT old = emu->viewport_rect;
				CalcUxnViewport(emu);
				/* If the viewport has changed, invalidate to paint over the potential junk pixels once. */
				if (!EqualRect(&old, &emu->viewport_rect)) InvalidateRect(emu->hWnd, &old, FALSE);
			}
			else RefitEmuWindow(emu);
		}
		return;
	}
	switch (device)
	{
	case VV_SYSTEM:
		switch (port)
		{
		case 0x3:
		{
			UINT offset, i; BYTE *ram;
			struct { USHORT size, a_slot, a_offset, b_slot, b_offset; } params = {0};
			DEVPEEK(imem, offset, 0x2);
			if (offset > UXN_RAM_SIZE - (1 + sizeof params)) goto stasher_fault;
			for (ram = box->core.ram + 1 + offset, i = 0; i < sizeof params / sizeof(USHORT); i++, ram += 2)
				((USHORT *)&params)[i] = GET_16BIT(ram);
			if (params.a_offset + params.size > UXN_RAM_SIZE ||
			    params.b_offset + params.size > UXN_RAM_SIZE) goto stasher_fault;
			CopyMemory(GetStashMemory(box, params.b_slot) + params.b_offset,
			           GetStashMemory(box, params.a_slot) + params.a_offset, params.size);
			break;
			stasher_fault: box->core.fault = UXN_FAULT_STASHER; break;
		}

		case 0x5: /* Set window title. Limit to 256 chars. */
		{
			BYTE *ram = GetStashMemory(box, 0); DWORD offset, i, n;
			for (DEVPEEK(imem, offset, 0x4), i = offset, n = 0; i < UXN_RAM_SIZE && n++ < 256;)
				if (!ram[i++]) { SetWindowTextA(emu->hWnd, (CHAR *)ram + offset); break; }
			break;
		}
		case 0xE: box->core.fault = UXN_FAULT_DEBUG; break;
		case 0xF: box->core.fault = UXN_FAULT_QUIT; break;
		default: if (port > 0x7 && port < 0xE)
		{
			UxnU8 *addr = imem + 0x8;
			UxnScreen *p = &emu->screen;
			int i, shift;
			for (i = 0, shift = 4; i < 4; ++i, shift ^= 4)
			{
				UxnU8
					r = (addr[0 + i / 2] >> shift) & 0x0F,
					g = (addr[2 + i / 2] >> shift) & 0x0F,
					b = (addr[4 + i / 2] >> shift) & 0x0F;
				p->palette[i] = 0x0F000000 | r << 16 | g << 8 | b;
				p->palette[i] |= p->palette[i] << 4;
			}
		}
		}
		break;
	case VV_CONSOLE:
	{
		ConWindow *con; DWORD i; CHAR *buf;
		if (port < 0x8) break;
		if (!emu->consoleHWnd) CreateConsoleWindow(emu);
		if (!IsWindowVisible(emu->consoleHWnd)) ShowWindow(emu->consoleHWnd, SW_SHOWNOACTIVATE);
		con = (ConWindow *)GetWindowLongPtr(emu->consoleHWnd, GWLP_USERDATA);
		buf = con->buffer;
		if (con->count >= sizeof con->buffer - 3) /* Need room for up to 2 chars and a terminator */
			FlushUxnConsole(con, emu->consoleHWnd);
		if (!(i = con->count)) SetTimer(emu->consoleHWnd, TimerID_FlushConsole, 1, NULL);
		if (con->has_newline) buf[i++] = '\r', buf[i++] = '\n', con->has_newline = 0;
		if ((buf[i] = (UxnU8)value) == '\n') con->has_newline = 1;
		else i++;
		con->count = i;
		break;
	}
	case VV_AUDIO0: case VV_AUDIO1: case VV_AUDIO2: case VV_AUDIO3:
		DevOut_Audio(emu, device, port); break;
	case VV_FILE0: case VV_FILE1:
		DevOut_File(emu, device, port); break;
	}
}

static void UxnDeviceWrite(UxnCore *u, UINT address, UINT value)
{
	UxnBox *box = OUTER_OF(u, UxnBox, core);
	UxnScreen *screen = &OUTER_OF(box, EmuWindow, box)->screen;
	UINT device = address & 0xF0;
	UxnU8 *devmem = box->device_memory, *imem = devmem + device;
	devmem[address] = value;
	if (address == VV_SCREEN + 0xE)
	{
		LONG x, y, width = screen->width, layer = imem[0xE] & 0x40;
		UxnU8 *pixels = layer ? screen->fg : screen->bg;
		DEVPEEK2(imem, x, y, 0x8);
		if (x < width && y < screen->height) /* poke pixel */
			pixels[x + y * width] = imem[0xE] & 0x3;
		if (imem[0x6] & 0x01) DEVPOKE(imem, 0x8, x + 1); /* auto x+1 */
		if (imem[0x6] & 0x02) DEVPOKE(imem, 0xA, y + 1); /* auto y+1 */
		return;
	}
	if (address == VV_SCREEN + 0xF)
	{
		UINT x, y, addr, i, tmp, advance = imem[0x6], sprite = imem[0xF];
		UINT n = advance >> 4, dx = advance << 3 & 8, dy = advance << 2 & 8;
		UINT twobpp = sprite >> 7 & 1, daddr = (advance & 0x4) << (twobpp + 1);
		int color = sprite & 0xF, flipx = sprite & 0x10, flipy = sprite & 0x20;
		UxnU8 *layer_pixels = (sprite & 0x40) ? screen->fg : screen->bg;
		DEVPEEK2(imem, x, y, 0x8);
		DEVPEEK(imem, addr, 0xC);
		tmp = x + dx; DEVPOKE(imem, 0x8, tmp);
		tmp = y + dy; DEVPOKE(imem, 0xA, tmp);
		for (i = 0; i <= n; i++, x += dy, y += dx)
		{
			if ((tmp = addr, addr += daddr) >= UXN_RAM_SIZE) break;
			DrawUxnSprite(screen, layer_pixels, x, y, &u->ram[tmp], color, flipx, flipy, twobpp);
		}
		DEVPOKE(imem, 0xC, addr);
		return; /* TODO some perf warning if drawing same sprite redundantly on top of itself? */
	}
	UxnDeviceWrite_Cold(box, address, value);
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
	if (scale > 1) { in_out_mousepoint->x /= scale; in_out_mousepoint->y /= scale; }
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

static LinkedList emus_needing_work, emus_needing_vblank;
static int emu_window_count;

/* This function is a bit tricky. It reads from d->running and Uxn device RAM. Call it if either changed. e.g. when (un)pausing, or after a vector finishes. If you're about to destroy or free an EmuWindow, you also need to make sure it's removed from vblank linked list, so set d->running to false and then call this function. */
static void Update60hzTimerEnabled(EmuWindow *d)
{
	BOOL signal_resume, enabled = d->running && GET_16BIT(d->box.device_memory + VV_SCREEN);
	if (ListLinkUsed(&emus_needing_vblank, d, vblank_link) == enabled) return;
	if (WaitForSingleObject(VBlankMutex, INFINITE) != WAIT_OBJECT_0) goto thread_error;
	signal_resume = enabled && !emus_needing_vblank.front;
	if (enabled) ListPushBack(&emus_needing_vblank, d, vblank_link);
	else ListRemove(&emus_needing_vblank, d, vblank_link);
	if (!ReleaseMutex(VBlankMutex)) goto thread_error;
	if (signal_resume) SetEvent(ResumeTimerEvent);
	return;
thread_error:
	FatalBox("Critical error while handling thread mutex.");
}

static void ResetVM(EmuWindow *d)
{
	d->box.core.fault = 0;
	d->exec_state = 0;
	ZeroMemory(&d->box.work_stack, sizeof(UxnStack) * 2); /* optional for quick reload */
	ZeroMemory(d->box.device_memory, sizeof d->box.device_memory); /* optional for quick reload */
	DEVPOKE2(d->box.device_memory, VV_SCREEN + 0x2, d->screen.width, d->screen.height); /* Restore this in case ROM reads it */
	ResetStasher(&d->box);
	d->box.core.ram = GetStashMemory(&d->box, 0);
	ZeroMemory(d->screen.palette, sizeof d->screen.palette); /* optional for quick reload */
	ZeroMemory(d->screen.bg, d->screen.width * d->screen.height * 2);
	ResetFiler(&d->filers[0]); ResetFiler(&d->filers[1]);
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
	Update60hzTimerEnabled(d);
	SetHostCursorVisible(d, TRUE);
	ListRemove(&emus_needing_work, d, work_link);
}

static void UnpauseVM(EmuWindow *d)
{
	if (d->running) return;
	d->queue_count = 0;
	d->running = 1;
	Update60hzTimerEnabled(d);
	if (d->exec_state) ListPushBack(&emus_needing_work, d, work_link);
	SynthesizeMouseMoveToCurrent(d); /* Runs async for now */
	/* Syncing held keys isn't so easy... */
}

static void OpenBeetbugWindow(EmuWindow *emu, BOOL force)
{
	if (!emu->beetbugHWnd || force)
	{
		emu->beetbugHWnd = CreateWindowEx(
			0, BeetbugWinClass, TEXT("Beetbug"), WS_OVERLAPPEDWINDOW,
			CW_USEDEFAULT, CW_USEDEFAULT, 530, 395,
			emu->hWnd, NULL, MainInstance, emu);
	}
	if (!IsWindowVisible(emu->beetbugHWnd)) ShowWindow(emu->beetbugHWnd, SW_SHOW);
}

static void BeetbugAutoScrollStacks(BeetbugWin *dbg)
{
	int pp = ListView_GetCountPerPage(dbg->ctrls[BB_WrkStack]), pad = pp / 3, top, i, s;
	for (i = BB_WrkStack; i <= BB_RetStack; i++)
	{
		top = ListView_GetTopIndex(dbg->ctrls[i]);
		s = (&dbg->emu->box.core.wst)[i - BB_WrkStack]->num; /* actually points to 1 past the last value */
		if (s-- == 0 || (s >= top && s < top + pp)) continue;
		ListView_EnsureVisible(dbg->ctrls[i], MAX(0, s - pp), FALSE);
		ListView_EnsureVisible(dbg->ctrls[i], MIN(255, s + pad), FALSE);
	}
}

static void ShowBeetbugInstruction(EmuWindow *emu, USHORT address)
{
	BeetbugWin *dbg; int pad_rows;
	OpenBeetbugWindow(emu, FALSE);
	dbg = (BeetbugWin *)GetWindowLongPtr(emu->beetbugHWnd, GWLP_USERDATA);
	pad_rows = ListView_GetCountPerPage(dbg->ctrls[BB_AsmList]) / 3;
	ListView_EnsureVisible(dbg->ctrls[BB_AsmList], MAX(0, address - pad_rows), FALSE); /* TODO probably wasteful */
	ListView_EnsureVisible(dbg->ctrls[BB_AsmList], MIN(0xFFFF, address + pad_rows), FALSE);
	BeetbugAutoScrollStacks(dbg);
}

/* Pass non-zero steps to use fixed-stepping mode. Pass 0 to use normal run mode with an automatic number of steps. */
/* The 'initial' parameter indicates whether or not this is first call into RunUxn() for a
 * particular interrupt or not. All it does is check if the pc starts on address 0, and if
 * it does, skips trying to execute code and does the other work like scheduling, cleanup, etc.
 * This is kind of a dirty hack. Ideally we'd do something else (like split this routine into
 * smaller pieces and call the pieces we want) but since this will probably be overhauled in
 * the future to support executing multiple queued events without returning to the Windows event
 * loop, we don't want to make it more complicated just yet. */
static void RunUxn(EmuWindow *d, UINT steps, BOOL initial)
{
	UINT res, use_steps = steps ? steps : 100000;  /* about 1900 usecs on good hardware */
	UxnCore *u = &d->box.core; LONGLONG t_a, t_delta;
	int more_work, force_repaint;
	if (initial && !u->pc) goto completed;
	t_a = TimeStampNow();
	for (;;)
	{
		res = UxnExec(&d->box.core, use_steps);
		t_delta = TimeStampNow() - t_a;
		d->instr_count += use_steps - res;
		if (u->fault) break;
		if (t_delta > ExecutionTimeLimit || steps) goto residual;
	}
	if (u->fault)
	{
		UINT last_addr = ((UINT)u->pc - 1) % UXN_RAM_SIZE, last_op = u->ram[last_addr], fault_handler;
		DEVPEEK(d->box.device_memory, fault_handler, 0x0);
		if (fault_handler && u->fault <= UXN_FAULT_DIVIDE_BY_ZERO)
		{
			u->wst->num = 4;
			u->wst->mem[0] = last_addr >> 8, u->wst->mem[1] = last_addr;
			u->wst->mem[2] = last_op;
			u->wst->mem[3] = u->fault - 1;
			u->fault = 0;
			u->pc = fault_handler;
			goto residual;
		}
		if (u->fault == UXN_FAULT_DONE) goto completed;
		/* If there's a division by zero, push 0xFF onto the stack to rebalance it. Then, if the user hits resume, the program has a better chance of not faulting again. */
		if (u->fault == UXN_FAULT_DIVIDE_BY_ZERO)
		{
			UxnStack *s = last_op & 0x40 ? u->rst : u->wst; /* Which stack to push to */
			int i = 0, count = (last_op & 0x20) >> 5; /* Push 1 or 2 bytes */
			for (; i <= count; i++) s->mem[s->num++] = 0xFF;
		}
		PauseVM(d);
		/* This particular fault code means ROM program requested to 'quit'. What should we do?
		 * If Beetbug isn't open, then close the emulator window.
		 * If Beetbug is open, then let Beetbug show that the ROM wanted to quit. */
		if (u->fault == UXN_FAULT_QUIT && !IsWindowVisible(d->beetbugHWnd))
		{
			/* If Beetbug isn't open but the console window is open, don't quit. There might be useful messages there. */
			if (IsWindowVisible(d->consoleHWnd))
			{
				ConWindow *con = (ConWindow *)GetWindowLongPtr(d->consoleHWnd, GWLP_USERDATA); int len;
				FlushUxnConsole(con, d->consoleHWnd);
				len = GetWindowTextLength(con->outHWnd); /* TODO repetitive */
				SendMessage(con->outHWnd, EM_SETSEL, len, len);
				SendMessage(con->outHWnd, EM_REPLACESEL, 0, (LPARAM)TEXT("\r\n\r\n[Program quit]"));
			}
			else
			{
				PostMessage(d->hWnd, WM_CLOSE, 0, 0);
			}
			return;
		}
		InvalidateUxnScreenRect(d);
		ShowBeetbugInstruction(d, u->pc);
		return;
	}
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
	case EmuIn_DebugJump:
	case EmuIn_Start:
		break;
	case EmuIn_KeyChar:
		d->box.device_memory[VV_CONTROL + 0x3] = 0;
		break;
	case EmuIn_Wheel:
		DEVPOKE2(d->box.device_memory, VV_MOUSE + 0xA, 0, 0);
		break;
	}
	if (d->running) u->fault = 0;
	d->exec_state = 0;
	Update60hzTimerEnabled(d);
	/* ^- Screen vector might have been changed between null and non-null, or, we need to enable it for the first time after the Start event. */
residual:
	more_work = d->exec_state || d->queue_count;
	force_repaint = TimeStampNow() - d->last_paint > RepaintTimeLimit;
	if (more_work && d->running) ListPushBack(&emus_needing_work, d, work_link);
	if (!more_work || force_repaint) InvalidateUxnScreenRect(d);
	if (force_repaint) UpdateWindow(d->hWnd);
}

static void ApplyInputEvent(EmuWindow *d, BYTE type, BYTE bits, USHORT x, USHORT y)
{
	UxnU16 *pc = &d->box.core.pc; UxnU8 *devmem = d->box.device_memory;
	switch ((enum EmuIn)type)
	{
	case EmuIn_KeyChar:
		devmem[VV_CONTROL + 3] = bits;
		*pc = GET_16BIT(devmem + VV_CONTROL);
		break;
	case EmuIn_CtrlDown: devmem[VV_CONTROL + 2] |=  bits; goto run_ctrl;
	case EmuIn_CtrlUp:   devmem[VV_CONTROL + 2] &= ~bits; goto run_ctrl;
	case EmuIn_ResetKeys:
		/* If the requested keys held down match the existing, there's nothing more to do. */
		/* Can't skip RunUxn() since we might need to queue more work. TODO could factor out. */
		if (devmem[VV_CONTROL + 2] == bits) { *pc = 0; break; }
		devmem[VV_CONTROL + 2] = bits;
	run_ctrl:
		*pc = GET_16BIT(devmem + VV_CONTROL);
		break;
	case EmuIn_MouseDown:
		devmem[VV_MOUSE + 6] |= bits; goto mouse_xy;
	case EmuIn_MouseUp:
		devmem[VV_MOUSE + 6] &= ~bits;
	mouse_xy:
		DEVPOKE2(devmem + VV_MOUSE, 0x2, x, y);
		*pc = GET_16BIT(devmem + VV_MOUSE);
		break;
	case EmuIn_Wheel:
		DEVPOKE2(devmem + VV_MOUSE, 0xA, x, y);
		*pc = GET_16BIT(devmem + VV_MOUSE);
		break;
	case EmuIn_Screen:
		*pc = GET_16BIT(devmem + VV_SCREEN);
		break;
	case EmuIn_Console:
		devmem[VV_CONSOLE + 0x2] = bits;
		*pc = GET_16BIT(devmem + VV_CONSOLE);
		break;
	case EmuIn_DebugJump: break; /* Should not happen -- only set by debugger */
	case EmuIn_Start:
		*pc = UXN_ROM_OFFSET;
		break;
	}
	d->exec_state = type;
	if (*pc) { d->last_event = type; d->instr_count = 0; }
	/* ^- To make the debugger UI useful, don't update these if the program vector is empty */
	if (type == EmuIn_Start && BreakOnInitVector) { PauseVM(d); ShowBeetbugInstruction(d, *pc); return; }
	RunUxn(d, 0, TRUE);
}

static void SendInputEvent(EmuWindow *d, BYTE type, BYTE bits, USHORT x, USHORT y)
{
	if (!d->running) return;
	if (!d->exec_state && !d->queue_count) ApplyInputEvent(d, type, bits, x, y);
	else /* busy */
	{
		EmuInEvent *evt;
		if (d->queue_count == QUEUE_CAP) return; /* it's too crowded here anyway */
		if (!d->queue_buffer) d->queue_buffer = HeapAlloc0OrDie(QUEUE_CAP * sizeof(EmuInEvent));
		evt = d->queue_buffer + (d->queue_first + d->queue_count++) % QUEUE_CAP;
		evt->type = type; evt->bits = bits; evt->x = x; evt->y = y;
	}
}

static void LoadROMFileAndStartVM(EmuWindow *d)
{
	if (!LoadROMIntoBox(&d->box, d->rom_path))
	{
		/* Can't load ROM file? Load a small ROM to display an error screen. */
		HRSRC hInfo = FindResource(MainInstance, MAKEINTRESOURCE(IDR_FLUMMOX), TEXT("ROM"));
		DWORD rom_size = SizeofResource(MainInstance, hInfo);
		void *data = LockResource(LoadResource(MainInstance, hInfo));
		if (!data) return;
		CopyMemory(d->box.core.ram + UXN_ROM_OFFSET, data, MIN(rom_size, UXN_RAM_SIZE - UXN_ROM_OFFSET));
	}
	d->running = 1;
	SendInputEvent(d, EmuIn_Start, 0, 0, 0);
}

static void ReloadFromROMFile(EmuWindow *d)
{
	PauseVM(d);
	ResetVM(d);
	LoadROMFileAndStartVM(d);
	SynthesizeMouseMoveToCurrent(d); /* Still has a brief flicker of wrong cursor... oh well */
	/* We want to resync the held keys here, but it's not safe to do so, because we get garbage results from GetKeyState() and GetAsyncKeyState() if the open file dialog has just recently been closed by the user. There are also similar issues with syncing key state when reactivating the window by clicking on the title bar while holding modifiers. So forget about it for now.*/
	SendMessage(d->hWnd, UXNMSG_SendArgs, 0, 0);
	if (d->beetbugHWnd) SendMessage(d->beetbugHWnd, UXNMSG_LoadSymbols, 0, 0);
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

static INT_PTR CALLBACK AboutBoxProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	(void)lParam;
	switch (message)
	{
	case WM_INITDIALOG: return TRUE;
	case WM_COMMAND:
		if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)
			return EndDialog(hDlg, LOWORD(wParam));
		break;
	}
	return FALSE;
}

static HWND CreateWindowForEmu(HINSTANCE hInst, EmuWindow *emu)
{
	RECT rect;
	rect.left = rect.top = 0;
	rect.right = UXN_DEFAULT_WIDTH; rect.bottom = UXN_DEFAULT_HEIGHT;
	AdjustWindowRect(&rect, WS_OVERLAPPEDWINDOW, TRUE);
	return CreateWindowEx(WS_EX_APPWINDOW, EmuWinClass, TEXT("Uxn"), WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, rect.right - rect.left, rect.bottom - rect.top, NULL, NULL, hInst, (void *)emu);
}

static void CloneWindow(EmuWindow *a)
{
	EmuWindow *emu = HeapAlloc0OrDie(sizeof(EmuWindow));
	HWND hWnd = CreateWindowForEmu(MainInstance, emu);
	ShowWindow(hWnd, SW_SHOW);
	PostMessage(hWnd, UXNMSG_BecomeClone, (WPARAM)a, 0);
}

static LPCSTR const uxn_op_names =
TEXT("LITINCPOPNIPSWPROTDUPOVREQUNEQGTHLTHJMPJCNJSRSTHLDZSTZLDRSTRLDASTADEIDEOADDSUBMULDIVANDORAEORSFTBRKJCIJMIJSI");
/* Note: BRK is appended to the end for special-case code when encoding/decoding mnemonics. */

static int DecodeUxnOpcode(TCHAR *out, BYTE instr)
{
	/* BRK, JCI, JMI, and JSI are special. LIT is also a little special. */
	int n = 3, base_op = instr & 0x1F, is_normal = instr & ~0x60;
	int name_index = is_normal ? base_op : (instr >> 5 & 3) + 32;
	CopyMemory(out, uxn_op_names + name_index * 3, 3 * sizeof(TCHAR));
	if (is_normal)
	{
		if (instr & 0x20) out[n++] = '2';
		if (instr & 0x80 && base_op) out[n++] = 'k'; /* Don't show 'k' on LIT */
		if (instr & 0x40) out[n++] = 'r';
	}
	out[n] = 0;
	return n;
}

static BOOL IsSortaSpace(TCHAR c) { return c == ' ' || c == '\r' || c == '\n' || c == '\t'; }

static BOOL ParseHex(TCHAR const *in, UINT *out)
{
	UINT a = 0; TCHAR c;
	while (IsSortaSpace(*in)) in++;
	if (!*in) return FALSE; /* TODO sloppy around here */
	while ((c = *in++))
	{
		if      (c >= '0' && c <= '9') a = (a << 4) + c - '0';
		else if (c >= 'A' && c <= 'F') a = (a << 4) + c - 'A' + 10;
		else if (c >= 'a' && c <= 'f') a = (a << 4) + c - 'a' + 10;
		else if (a == 0 && (c == 'x' || c == '$' || c == '#')) continue;
		else if (c == 'h' || c == ' ' || c == '\t' || c == '\r' || c == '\n') break;
		else return FALSE;
	}
	*out = a;
	return TRUE;
}

static BOOL EncodeUxnOpcode(LPCSTR in, BYTE *out)
{
	int i, len; TCHAR tmp[6], c; UINT a = 0;
	while (IsSortaSpace(*in)) in++;
	for (i = 0; i < 6; i++)
	{
		if (!(tmp[i] = in[i]) || IsSortaSpace(tmp[i])) break;
		if (tmp[i] >= 'a' && tmp[i] <= 'z')
			tmp[i] = tmp[i] - ('a' - 'A');
	}
	if (!(len = i)) return FALSE;
	if (len < 3) /* Try as hex */
	{
		for (i = 0; i < len; i++)
		{
			if ((c = tmp[i]) >= '0' && c <= '9') a = (a << 4) + c - '0';
			else if       (c >= 'A' && c <= 'F') a = (a << 4) + c - 'A' + 10;
			else return FALSE;
		}
		*out = (BYTE)a;
		return TRUE;
	}
	/* Try as mnemonic */
	for (in = uxn_op_names; *in; in += 3, a++)
		if (in[0] == tmp[0] && in[1] == tmp[1] && in[2] == tmp[2])
			goto found;
	return FALSE;
found:
	/* For 'LIT', add an implicit 'k' modifier. For special ops, offset their position. */
	a = !a ? 0x80 : a >= 32 ? (a - 32) << 5 : a;
	for (i = 3; i < len; i++)
		if      (tmp[i] == '2') a |= 0x20;
		else if (tmp[i] == 'K') a |= 0x80;
		else if (tmp[i] == 'R') a |= 0x40;
		else return FALSE;
	*out = (BYTE)a;
	return TRUE;
}

static BOOL LoadUxnDebugSymbols(LPCTSTR path, UxnDebugSymbols *out)
{
	DWORD bytes_read, file_size;
	UINT entry_count, addrs_size, i, e;
	BYTE *buff = NULL; USHORT *addresses; CHAR **strings;
	BOOL result = FALSE;
	HANDLE hFile = CreateFile(path, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hFile == INVALID_HANDLE_VALUE) return FALSE;
	if ((file_size = GetFileSize(hFile, NULL)) == 0xFFFFFFFF) goto fail;
	buff = HeapAlloc0OrDie(file_size);
	if (!ReadFile(hFile, buff, file_size, &bytes_read, NULL) || bytes_read != file_size) goto fail;
	for (entry_count = i = 0; i < file_size; entry_count++) /* Find number of entries */
	{
		for (i += 2;;) /* Advance past a 16-bit value, then check for a null terminated string */
		{
			if (i >= file_size) goto fail; /* Bad: ragged file or not null terminated string */
			if (!buff[i++]) break; /* If we find the string's null terminator, move to the next entry. */
		}
	}
	if (entry_count > (UINT)-1 / (sizeof(CHAR *) * 2)) goto fail; /* Some reasonable upper limit */
	addrs_size = entry_count * sizeof(USHORT) + sizeof(CHAR *) - 1 & ~(sizeof(CHAR *) - 1);
	/* ^ Size needed to hold array of 16-bit addresses, plus pointer-aligning padding. */
	addresses = entry_count ? HeapAlloc0OrDie(addrs_size + entry_count * sizeof(CHAR *)) : NULL;
	/* ^ Buffer for: array of 16-bit addresses + pad to pointer + array of char pointers */
	strings = (CHAR **)((BYTE *)addresses + addrs_size); /* The char pointers array */
	for (i = e = 0; e < entry_count; e++) /* Set the values in the addresses and char pointers arrays */
	{
		addresses[e] = (buff[i] << 8) + buff[i + 1];
		for (strings[e] = (CHAR *)buff + (i += 2); buff[i++];);
	}
	for (i = 1; i < entry_count; i++) /* Insertion sort, to make it binary searchable */
	{
		USHORT t = addresses[i]; CHAR *u = strings[i]; /* tmp vars */
		for (e = i; e > 0 && addresses[e - 1] > t; e--)
		{
			addresses[e] = addresses[e - 1], addresses[e - 1] = t;
			strings[e] = strings[e - 1], strings[e - 1] = u;
		}
	}
	out->count = entry_count; out->addresses = addresses; out->strings = strings; out->buffer = buff;
	result = TRUE;
done:
	CloseHandle(hFile);
	return result;
fail:
	HeapFree0(buff);
	goto done;
}

static void FreeUxnDebugSymbols(UxnDebugSymbols *s)
{
	HeapFree0(s->buffer);
	HeapFree0(s->addresses);
}

/* Returns the index (0 through UINT_MAX-1) for the symbol whose address that's equal to or earlier than 'address'.
 * Returns (UINT)-1 if no symbol was found. */
static UINT FindSymbolForAddress(UxnDebugSymbols *s, USHORT address)
{
	USHORT *addrs = s->addresses;
	UINT count = s->count, first = 0, i, step;
	while (count > 0) /* Binary search */
	{
		step = count / 2;
		i = first + step;
		if (addrs[i] <= address) { first = ++i; count -= step + 1; }
		else count = step;
	}
	return first - 1;
}

static void UpdateBeetbugStuff(HWND hWnd, BeetbugWin *d)
{
	static const LPCSTR play_texts[] = {0, TEXT("Running"), TEXT("Suspended"), TEXT("Paused")};
	BYTE new_play = d->emu->running ? 1 : d->emu->exec_state ? 2 : 3;
	BOOL step_ctrls = new_play == 2; TCHAR buff[128]; int i;
	/* int top = ListView_GetTopIndex(d->hList), bot = top + ListView_GetCountPerPage(d->hList); */
	/* TODO ust ListView_RedrawItems() instead? */
	for (i = BB_AsmList; i <= BB_DevMem; i++) InvalidateRect(d->ctrls[i], NULL, FALSE); /* TODO only changed areas */
	for (i = 0; i < 2; i++) InvalidateRect(hWnd, &d->rcWstLabel + i, FALSE);
	if (d->sbar_play_mode != new_play)
	{
		if (d->sbar_play_mode) d->sbar_flashing |= 1 << 0, SetTimer(hWnd, BB_Status + 0, 250, NULL);
		SendMessage(d->ctrls[BB_Status], SB_SETTEXT, 0 | SBT_OWNERDRAW, (LPARAM)(play_texts[d->sbar_play_mode = new_play]));
		SetWindowText(d->ctrls[BB_PauseBtn], new_play == 1 ? TEXT("Pause (F9)") : TEXT("Resume (F9)"));
	}
	if (d->sbar_input_event != d->emu->last_event)
	{
		static const LPCSTR event_texts[EmuIn_Start + 1] = { /* TODO crappy */
			TEXT(""), TEXT("KeyChar"), TEXT("CtrlDown"), TEXT("CtrlUp"),
			TEXT("CtrlUp"), TEXT("Mouse"), TEXT("MouseUp"), TEXT("Wheel"),
			TEXT("Screen"), TEXT("ConChar"), TEXT("DebugJump"), TEXT("Init")
		};
		SendMessage(d->ctrls[BB_Status], SB_SETTEXT, 1,
			(LPARAM)event_texts[d->sbar_input_event = d->emu->last_event]);
	}
	if (d->sbar_pc != d->emu->box.core.pc)
	{
		wsprintf(buff, "PC: %04X", (UINT)(d->sbar_pc = d->emu->box.core.pc));
		SendMessage(d->ctrls[BB_Status], SB_SETTEXT, 2, (LPARAM)buff);
	}
	if (d->sbar_instrcount != d->emu->instr_count)
	{
		UINT u = (d->sbar_instrcount = d->emu->instr_count) > (UINT)-1 ?
			(UINT)-1 : (UINT)d->sbar_instrcount;
		wsprintf(buff, "Ops: %u", u);
		SendMessage(d->ctrls[BB_Status], SB_SETTEXT, 3, (LPARAM)buff);
	}
	if (d->sbar_fault != d->emu->box.core.fault)
	{
		LPCSTR text = NULL;
		switch (d->sbar_fault = d->emu->box.core.fault)
		{
		case UXN_FAULT_DONE: text = TEXT("Break"); break;
		case UXN_FAULT_STACK_UNDERFLOW: text = TEXT("Stack underflow"); break;
		case UXN_FAULT_STACK_OVERFLOW: text = TEXT("Stack overflow"); break;
		case UXN_FAULT_DIVIDE_BY_ZERO: text = TEXT("Division by zero"); break;
		/* case 127: text = TEXT("Stack debris"); break; */ /* TODO search debris */
		case UXN_FAULT_STASHER: text = TEXT("Stasher fault"); break;
		case UXN_FAULT_DEBUG: text = TEXT("Debug device break"); break;
		case UXN_FAULT_QUIT: text = TEXT("Program requested exit"); break;
		}
		if (d->sbar_fault) d->sbar_flashing |= 1 << 4, SetTimer(hWnd, BB_Status + 4, 250, NULL);
		SendMessage(d->ctrls[BB_Status], SB_SETTEXT, 4 | SBT_OWNERDRAW, (LPARAM)text);
	}
	if (IsWindowEnabled(d->ctrls[BB_BigStepBtn]) != step_ctrls)
	{
		EnableWindow(d->ctrls[BB_BigStepBtn], step_ctrls);
		EnableWindow(d->ctrls[BB_StepBtn], step_ctrls);
	}
	for (i = 0; i < 2; i++)
	{
		int p = (&d->emu->box.core.wst)[i]->num;
		BOOL ok_push = p < 255, ok_pop = p > 0;
		if (IsWindowEnabled(d->ctrls[BB_PushStackBtn0 + i]) != ok_push)
			EnableWindow(d->ctrls[BB_PushStackBtn0 + i], ok_push);
		if (IsWindowEnabled(d->ctrls[BB_PopStackBtn0 + i]) != ok_pop)
			EnableWindow(d->ctrls[BB_PopStackBtn0 + i], ok_pop);
	}
}

static LRESULT CALLBACK BeetbugJumpEditProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	if (msg == WM_KEYDOWN && wParam == VK_RETURN)
		return SendMessage(GetParent(hWnd), WM_COMMAND, MAKEWPARAM(BB_JumpBtn, BN_CLICKED), (LPARAM)hWnd);
	if (msg == WM_COMMAND && LOWORD(wParam) == IDM_SELECTALL)
		return SendMessage(hWnd, EM_SETSEL, 0, -1);
	return CallWindowProc((WNDPROC)GetWindowLongPtr(hWnd, GWLP_USERDATA), hWnd, msg, wParam, lParam);
}

static LRESULT CALLBACK BeetbugWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	BeetbugWin *d = (BeetbugWin *)GetWindowLongPtr(hWnd, GWLP_USERDATA);
	switch (msg)
	{
	case WM_CREATE:
	{
		LONG_PTR i, j; HWND list; LV_COLUMN col; HFONT hFont = GetSmallFixedFont();
		static const int
			columns[] = { /* Instr list */ 45 + 25 + 50 + 90, /* Hex list */ 40 + 130,
			              /* Stacks */ 25, 25, /* Device mem */ 20 + 130},
			rows[] = {UXN_RAM_SIZE, UXN_RAM_SIZE / 8, 255, 255, 256 / 8},
			status_parts[] = {70, 140, 200, 300, -1};
		d = HeapAlloc0OrDie(sizeof *d);
		SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG_PTR)d);
		d->emu = ((CREATESTRUCT *)lParam)->lpCreateParams;
		ZeroMemory(&col, sizeof col);
		col.mask = LVCF_FMT | LVCF_WIDTH;
		for (i = j = 0; i < 5; i++)
		{
			d->ctrls[BB_AsmList + i] = list = CreateWindowEx(
				WS_EX_CLIENTEDGE, WC_LISTVIEW, NULL,
				WS_TABSTOP | WS_CHILD | WS_BORDER | WS_VISIBLE |
					LVS_REPORT | LVS_OWNERDATA | LVS_SHOWSELALWAYS |
					LVS_NOCOLUMNHEADER | LVS_EDITLABELS | LVS_SINGLESEL,
				0, 0, 0, 0, hWnd, (HMENU)(i + BB_AsmList), MainInstance, NULL);
			if (hFont) SendMessage(list, WM_SETFONT, (WPARAM)hFont, 0);
			ListView_SetExtendedListViewStyle(list, LVS_EX_FULLROWSELECT);
			ListView_DeleteAllItems(list);
			col.cx = columns[i]; ListView_InsertColumn(list, 0, &col);
			SendMessage(list, LVM_SETITEMCOUNT, rows[i], LVSICF_NOSCROLL);
		}
		d->ctrls[BB_Status] = CreateWindowEx(
			0, STATUSCLASSNAME, NULL, WS_CHILD | WS_VISIBLE | SBARS_SIZEGRIP,
			0, 0, 0, 0, hWnd, (HMENU)BB_Status, MainInstance, NULL);
		SendMessage(d->ctrls[BB_Status], SB_SETPARTS, sizeof status_parts / sizeof(int), (LPARAM)status_parts);
		for (i = BB_BigStepBtn; i <= BB_PopStackBtn1; i++)
		{
			HWND btn = d->ctrls[i] = CreateWindowEx(0, TEXT("Button"), NULL, WS_TABSTOP | WS_VISIBLE | WS_CHILD, 0, 0, 0, 0, hWnd, (HMENU)(i), MainInstance, NULL);
			SendMessage(btn, WM_SETFONT, i < BB_PushStackBtn0 ? (WPARAM)GetStockObject(DEFAULT_GUI_FONT) : (WPARAM)hFont, 0);
		}
		SetWindowText(d->ctrls[BB_BigStepBtn], TEXT("Big Step (F7)"));
		SetWindowText(d->ctrls[BB_StepBtn], TEXT("Step (F8)"));
		SetWindowText(d->ctrls[BB_JumpBtn], TEXT("View Address"));
		for (i = BB_PushStackBtn0; i <= BB_PopStackBtn1; i++)
			SetWindowText(d->ctrls[i], i < BB_PopStackBtn0 ? TEXT("PUSH") : TEXT("POP"));
		d->ctrls[BB_JumpEdit] = CreateWindowEx(WS_EX_CLIENTEDGE, EditWinClass, NULL,
			WS_CHILD | WS_VISIBLE, 0, 0, 0, 0, hWnd, (HMENU)BB_JumpEdit, MainInstance, NULL);
		SetWindowLongPtr(d->ctrls[BB_JumpEdit], GWLP_USERDATA, (LONG_PTR)GetWindowLongPtr(d->ctrls[BB_JumpEdit], GWLP_WNDPROC));
		SetWindowLongPtr(d->ctrls[BB_JumpEdit], GWLP_WNDPROC,  (LONG_PTR)BeetbugJumpEditProc);
		SendMessage(d->ctrls[BB_JumpEdit], EM_SETLIMITTEXT, 4, 0);
		SendMessage(d->ctrls[BB_JumpEdit], WM_SETFONT, (WPARAM)hFont, 0);
		SendMessage(hWnd, UXNMSG_LoadSymbols, 0, 0);
		UpdateBeetbugStuff(hWnd, d);
		SetTimer(hWnd, 1, 50, NULL);
		break;
	}
	case WM_CLOSE:
		ShowWindow(hWnd, SW_HIDE);
		return 0;
	case WM_DESTROY:
		FreeUxnDebugSymbols(&d->symbols);
		HeapFree0(d);
		break;
	case WM_SIZE:
	{
		static const SIZE btnSize = {87, 19}; int i; RECT r, tmp, tmp2;
		SetRect(&r, 0, 0, LOWORD(lParam), HIWORD(lParam));
		SendMessage(d->ctrls[BB_Status], WM_SIZE, 0, 0);
		GetClientRect(d->ctrls[BB_Status], &tmp);
		MapWindowPoints(d->ctrls[BB_Status], hWnd, (LPPOINT)&tmp, 2); /* must violate strict aliasing */
		r.bottom = tmp.top;
		CutRect(&r, FromLeft, 250, &tmp);
		CutRect(&tmp, FromBottom, 15, &tmp2);
		CutRectForWindow(&tmp2, FromLeft, 50, d->ctrls[BB_JumpEdit]);
		MoveWindowRect(d->ctrls[BB_JumpBtn], &tmp2, TRUE);
		CutRectForWindow(&tmp, FromBottom, (tmp.bottom - tmp.top) * 10 / 25, d->ctrls[BB_HexList]);
		MoveWindowRect(d->ctrls[BB_AsmList], &tmp, TRUE);
		d->rcBlank = r;
		r.top += 5;
		CutRect(&r, FromTop, btnSize.cy, &tmp);
		r.top += 5;
		d->rcBlank.bottom = r.top; /* TODO fix filling over buttons by using regions? */
		tmp.left += 5; tmp.right = tmp.left + 500;
		for (i = BB_BigStepBtn; i <= BB_PauseBtn; i++)
		{
			CutRectForWindow(&tmp, FromLeft, btnSize.cx, d->ctrls[i]);
			tmp.right--;
		}
		for (i = 0; i < 2; i++)
		{
			CutRect(&r, FromLeft, 50, &tmp);
			CutRect(&tmp, FromBottom, 20, &d->rcWstLabel + i);
			CutRectForWindow(&tmp, FromBottom, 15, d->ctrls[BB_PopStackBtn0  + i]);
			CutRectForWindow(&tmp, FromBottom, 15, d->ctrls[BB_PushStackBtn0 + i]);
			MoveWindowRect(d->ctrls[BB_WrkStack + i], &tmp, TRUE);
		}
		CutRect(&r, FromBottom, 20, &d->rcDevMemLabel);
		MoveWindowRect(d->ctrls[BB_DevMem], &r, TRUE);
		break;
	}
	case WM_PAINT:
	{
		/* TODO do we actually need this? just use background painting? */
		static LPCSTR st_labels[] = {TEXT("WST"), TEXT("RST")}, str;
		RECT rTmp; PAINTSTRUCT ps; TCHAR buff[32]; int i, old_bkmode;
		HGDIOBJ old_font; HDC hDC = BeginPaint(hWnd, &ps);
		if (IntersectRect(&rTmp, &ps.rcPaint, &d->rcBlank))
			FillRect(hDC, &rTmp, (HBRUSH)(COLOR_3DFACE + 1));
		old_bkmode = SetBkMode(hDC, TRANSPARENT);
		old_font = SelectObject(hDC, GetSmallFixedFont());
		for (i = 0; i < 3; i++)
		{
			RECT *rc = &d->rcWstLabel + i;
			if (!IntersectRect(&rTmp, &ps.rcPaint, rc)) continue;
			FillRect(hDC, &rTmp, (HBRUSH)(COLOR_3DFACE + 1));
			if (i < 2)
			{
				wsprintf(buff, "%s %02X", st_labels[i], (UINT)(&d->emu->box.core.wst)[i]->num);
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
	case WM_DRAWITEM:
		if (wParam == BB_Status) /* Sometimes, some status bar parts flash by drawing inverted */
		{
			DRAWITEMSTRUCT *dis = (DRAWITEMSTRUCT *)lParam;
			DrawStatusText(dis->hDC, &dis->rcItem, (LPCSTR)dis->itemData, SBT_NOBORDERS);
			if (d->sbar_flashing & (1 << dis->itemID)) InvertRect(dis->hDC, &dis->rcItem);
			return 0;
		}
		break;
	case WM_ACTIVATE:
		if (wParam != WA_INACTIVE) SetFocus(d->ctrls[BB_AsmList]);
		return 0;
	case WM_NOTIFY:
		switch (((LPNMHDR)lParam)->code)
		{
		case NM_CUSTOMDRAW:
		{
			NMLVCUSTOMDRAW *cdraw = (NMLVCUSTOMDRAW *)lParam; RECT r; UxnStack *stack;
			if (wParam != BB_WrkStack && wParam != BB_RetStack) break;
			stack = (&d->emu->box.core.wst)[wParam - BB_WrkStack];
			switch (cdraw->nmcd.dwDrawStage)
			{
			case CDDS_PREPAINT:
				return CDRF_NOTIFYITEMDRAW | CDRF_NOTIFYPOSTPAINT;
			case CDDS_POSTPAINT: /* clean up line junk left at top when scrolling */
				if (ListView_GetTopIndex(d->ctrls[wParam]) == stack->num)
					return CDRF_NEWFONT;
				r.left = 0, r.top = 0, r.right = cdraw->nmcd.rc.right, r.bottom = 2;
				FillRect(cdraw->nmcd.hdc, &r, GetSysColorBrush(COLOR_WINDOW));
				return CDRF_NEWFONT;
			case CDDS_ITEMPREPAINT:
				cdraw->clrText = cdraw->nmcd.dwItemSpec < stack->num ?
					GetSysColor(COLOR_WINDOWTEXT) : GetSysColor(COLOR_GRAYTEXT);
				return CDRF_NEWFONT | (stack->num == cdraw->nmcd.dwItemSpec + 1 ? CDRF_NOTIFYPOSTPAINT : 0);
			case CDDS_ITEMPOSTPAINT:
				if (cdraw->nmcd.dwItemSpec + 1 != stack->num) return 0;
				if (!ListView_GetItemRect(d->ctrls[wParam], cdraw->nmcd.dwItemSpec, &r, LVIR_LABEL)) return 0;
				r.top = r.bottom - 1;
				FillRect(cdraw->nmcd.hdc, &r, GetSysColorBrush(COLOR_WINDOWTEXT));
				return CDRF_NEWFONT;
			}
			break;
		}

		case LVN_MARQUEEBEGIN: return -1; /* Disable. Broken in Win10 1809 w/o manifest, WTF? */

		{
		HWND hEdit; TCHAR buff[256]; int i;
		case NM_RETURN:
			hEdit = ListView_EditLabel(d->ctrls[wParam], ListView_GetNextItem(d->ctrls[wParam], -1, LVNI_SELECTED));
			goto custom_edit;
		case NM_DBLCLK:
			hEdit = ListView_EditLabel(d->ctrls[wParam], ((NMITEMACTIVATE *)lParam)->iItem);
		custom_edit:
			if (!hEdit || wParam != BB_AsmList) return 0;
			/* Using ((NMITEMACTIVATE *)lParam)->iItem to get the item index seems busted in the NM_RETURN case. Tends to use whatever the last time that was double clicked was. Shrug. Use the text in the thing given to us instead. */
			GetWindowText(hEdit, buff, 256);
			for (i = 0; i < 10; i++) buff[i] = ' '; /* Wipe over everything before the mnemonic with a space. */
			while (buff[i] && buff[i] != ' ') i++; /* Trim anything after opcode mnemonic, like a symbol name. */
			buff[i] = 0;
			SetWindowText(hEdit, buff);
			SendMessage(hEdit, EM_SETSEL, 10, -1);
			return 0;
		}

		case LVN_BEGINLABELEDIT:
			if (wParam == BB_WrkStack || wParam == BB_RetStack || wParam == BB_AsmList) return FALSE; /* false means ok */
			return TRUE;
		case LVN_ENDLABELEDIT:
		{
			NMLVDISPINFO *inf = (NMLVDISPINFO *)lParam; BYTE *base;
			if (!inf->item.pszText) return FALSE;
			if (wParam == BB_AsmList)
				base = GetStashMemory(&d->emu->box, 0);
			else if (wParam == BB_WrkStack || wParam == BB_RetStack)
				base = (&d->emu->box.core.wst)[wParam - BB_WrkStack]->mem;
			else return FALSE;
			if (!EncodeUxnOpcode(inf->item.pszText, base + inf->item.iItem)) return FALSE;
			return TRUE;
		}
		case LVN_GETDISPINFO:
		{
			TCHAR buff[256]; UxnCore *core = &d->emu->box.core; LV_DISPINFO *di = (LV_DISPINFO *)lParam;
			UINT iItem = di->item.iItem, addr = iItem, sym; BYTE *mem; UxnStack *stack;
			if (!(di->item.mask & LVIF_TEXT)) return 0;
			buff[0] = 0;
			switch (wParam)
			{
			case BB_AsmList:
				mem = GetStashMemory(&d->emu->box, 0);
				iItem = wsprintf(buff, "%c %04X %02X ", core->pc == addr ? '>' : ' ', (UINT)addr, (UINT)mem[addr]);
				iItem += DecodeUxnOpcode(buff + iItem, mem[addr]);
				if ((sym = FindSymbolForAddress(&d->symbols, addr)) < d->symbols.count && d->symbols.addresses[sym] == addr)
				{
					while (iItem < 17) buff[iItem++] = ' ';
					lstrcpynA(buff + iItem, d->symbols.strings[sym], 256 - iItem);
				}
				break;
			case BB_HexList:
				addr *= 8; mem = GetStashMemory(&d->emu->box, 0) + addr;
				wsprintf(buff, "%04X  %02X%02X %02X%02X %02X%02X %02X%02X", addr,
					(UINT)mem[0], (UINT)mem[1], (UINT)mem[2], (UINT)mem[3],
					(UINT)mem[4], (UINT)mem[5], (UINT)mem[6], (UINT)mem[7]);
				break;
			case BB_WrkStack: case BB_RetStack:
				stack = (&core->wst)[wParam - BB_WrkStack];
				wsprintf(buff, "%02X", (UINT)stack->mem[addr]);
				break;
			case BB_DevMem:
				addr *= 8; mem = d->emu->box.device_memory + addr;
				wsprintf(buff, "%02X  %02X%02X %02X%02X %02X%02X %02X%02X", addr,
					(UINT)mem[0], (UINT)mem[1], (UINT)mem[2], (UINT)mem[3],
					(UINT)mem[4], (UINT)mem[5], (UINT)mem[6], (UINT)mem[7]);
				break;
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
			case BB_StepBtn: idm = IDM_STEP; break;
			case BB_BigStepBtn: idm = IDM_BIGSTEP; break;
			case BB_PauseBtn: return SendMessage(d->emu->hWnd, WM_COMMAND, MAKEWPARAM(IDM_PAUSE, 0), 0);
			case BB_JumpBtn:
			{
				TCHAR buff[8]; UINT addr; HWND hList = d->ctrls[BB_HexList];
				GetWindowText(d->ctrls[BB_JumpEdit], buff, 8);
				if (!ParseHex(buff, &addr)) return 0;
				if (GetKeyState(VK_SHIFT) & 0x8000) /* experimental feature: view in asm list */
				{
					if (GetKeyState(VK_CONTROL) & 0x8000) /* experimental feature: jump pc to address */
					{
						d->emu->box.core.pc = addr;
						if (!d->emu->exec_state)
						{
							PauseVM(d->emu); /* temp hacks, might remove this feature later, dunno */
							d->emu->last_event = d->emu->exec_state = EmuIn_DebugJump;
							d->emu->instr_count = 0;
						}
					}
					UpdateBeetbugStuff(hWnd, d); /* kinda dumb, repeated a few times */
					ShowBeetbugInstruction(d->emu, addr);
				}
				else
				{
					ListView_EnsureVisible(hList, (addr /= 8) + ListView_GetCountPerPage(hList), FALSE);
					ListView_EnsureVisible(hList, addr, FALSE);
				}
				return 0;
			}
			case BB_PushStackBtn0: case BB_PushStackBtn1: case BB_PopStackBtn0: case BB_PopStackBtn1:
			{
				enum {Flags = LVIS_SELECTED | LVIS_FOCUSED}; int i;
				int push = idm <= BB_PushStackBtn1, iList = idm - (push ? BB_PushStackBtn0 : BB_PopStackBtn0);
				HWND hList = d->ctrls[BB_WrkStack + iList]; UxnStack *stack = (&d->emu->box.core.wst)[iList];
				if (stack->num == (push ? 255 : 0)) break;
				i = push ? stack->num++ : --stack->num - 1; if (i < 0) i = 0;
				ListView_SetItemState(hList, i, Flags, Flags);
				SetFocus(hList);
				if (push) ListView_EditLabel(hList, i);
				InvalidateRect(hList, NULL, FALSE);
				return 0;
			}

			}
		if ((idm == IDM_STEP || idm == IDM_BIGSTEP) && !d->emu->running && d->emu->exec_state)
		{
			d->emu->box.core.fault = 0;
			RunUxn(d->emu, idm == IDM_STEP ? 1 : 100, FALSE);
			UpdateBeetbugStuff(hWnd, d);
			ShowBeetbugInstruction(d->emu, d->emu->box.core.pc);
			return 0;
		}
		break;
	}
	case WM_TIMER:
		switch (wParam)
		{
		RECT rc;
		case 1: UpdateBeetbugStuff(hWnd, d); return 0;
		case BB_Status + 0: case BB_Status + 4: /* Timers for unflashing status bar parts */
			KillTimer(hWnd, wParam);
			d->sbar_flashing &= ~(1 << (wParam - BB_Status)); /* Clear flashing flag bit for this item */
			SendMessage(d->ctrls[BB_Status], SB_GETRECT, wParam - BB_Status, (LPARAM)&rc);
			InvalidateRect(d->ctrls[BB_Status], &rc, FALSE);
			return 0;
		}
		break;
	case UXNMSG_LoadSymbols:
	{
		TCHAR tmp[MAX_PATH]; int pathlen = lstrlen(d->emu->rom_path);
		FreeUxnDebugSymbols(&d->symbols); /* wasted work on first time... hmm */
		ZeroMemory(&d->symbols, sizeof d->symbols); /* also */
		if (pathlen < MAX_PATH - 5)
		{
			CopyMemory(tmp, d->emu->rom_path, pathlen * sizeof(TCHAR));
			CopyMemory(tmp + pathlen, TEXT(".sym"), 5 * sizeof(TCHAR));
			LoadUxnDebugSymbols(tmp, &d->symbols);
		}
	}
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
		d = HeapAlloc0OrDie(sizeof *d);
		SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG_PTR)d);
		d->outHWnd = CreateWindowEx(
			WS_EX_STATICEDGE, EditWinClass, NULL,
			WS_CHILD | WS_VISIBLE | WS_VSCROLL | ES_MULTILINE | ES_AUTOVSCROLL | ES_READONLY,
			0, 0, 0, 0, hWnd, (HMENU)1, MainInstance, NULL);
		d->inHWnd = CreateWindowEx(
			WS_EX_CLIENTEDGE, EditWinClass, NULL,
			WS_CHILD | WS_VISIBLE,
			0, 0, 0, 0, hWnd, (HMENU)2, MainInstance, NULL);
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
		KillTimer(hWnd, TimerID_FlushConsole);
		HeapFree0(d);
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
	case WM_TIMER:
		switch (wParam)
		{
		case TimerID_FlushConsole:
			FlushUxnConsole(d, hWnd);
			return 0;
		}
		break;
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
		UxnBox *box;
		d = ((CREATESTRUCT *)lparam)->lpCreateParams;
		emu_window_count++;
		SetWindowLongPtr(hwnd, GWLP_USERDATA, (LONG_PTR)d);
		DragAcceptFiles(hwnd, TRUE);
		box = &d->box;
		box->table = VirtualAlloc(NULL, sizeof(UxnStashPtr) * (USHORT)-1, MEM_RESERVE, PAGE_NOACCESS);
		box->core.ram = GetStashMemory(box, 0);
		box->core.wst = &box->work_stack;
		box->core.rst = &box->ret_stack;
		box->core.dei = UxnDeviceRead;
		box->core.deo = UxnDeviceWrite;
		d->host_cursor = TRUE;
		d->hWnd = hwnd; /* TODO cleanup reorder these assignments */
		d->viewport_scale = 1;
		SetUxnScreenSize(&d->screen, UXN_DEFAULT_WIDTH, UXN_DEFAULT_HEIGHT);
		d->filers[0].hFile = d->filers[0].hFind = d->filers[1].hFile = d->filers[1].hFind = INVALID_HANDLE_VALUE;
		if (lstrlen(d->rom_path)) LoadROMFileAndStartVM(d);
		return 0;
	}
	case WM_CLOSE:
		DestroyWindow(hwnd);
		return 0;
	case WM_DESTROY:
		d->running = FALSE, Update60hzTimerEnabled(d); /* it reads d->running */
		SetHostCursorVisible(d, TRUE);
		ResetStasher(&d->box);
		VirtualFree(d->box.table, 0, MEM_RELEASE);
		FreeUxnScreen(&d->screen);
		ResetFiler(&d->filers[0]);
		ResetFiler(&d->filers[1]);
		if (d->hHiddenMenu) DestroyMenu(d->hHiddenMenu);
		if (d->hBMP) DeleteObject(d->hBMP);
		if (d->hDibDC) DeleteDC(d->hDibDC);
		if (d->wave_out)
		{
			if (d->wave_out->hWaveOut)
			{
				waveOutPause(d->wave_out->hWaveOut);
				waveOutClose(d->wave_out->hWaveOut);
			}
			HeapFree0(d->wave_out);
		}
		ListRemove(&emus_needing_work, d, work_link);
		HeapFree0(d);
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
		MINMAXINFO *info = (MINMAXINFO *)lparam; RECT c;
		/* Use minimum width of 185 to prevent menu bar wrapping. */
		c.left = c.top = c.bottom = 0; c.right = 185;
		/* Note: WM_GETMINMAXINFO may be sent before WM_CREATE! */
		/* Use 1x Uxn screen scale when calculating minimum window size. */
		if (d) { c.right = MAX(c.right, d->screen.width); c.bottom = d->screen.height; }
		AdjustWindowRect(&c, GetWindowLong(hwnd, GWL_STYLE), d && !d->hHiddenMenu);
		info->ptMinTrackSize.x = c.right - c.left;
		info->ptMinTrackSize.y = c.bottom - c.top;
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
		mouse.x = GET_X_LPARAM(lparam); mouse.y = GET_Y_LPARAM(lparam);
		mouse_in_uxn = PtInRect(&d->viewport_rect, mouse) && d->running && GET_16BIT(d->box.device_memory + VV_MOUSE);
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
		mouse.x = GET_X_LPARAM(lparam); mouse.y = GET_Y_LPARAM(lparam);
		ScreenToClient(hwnd, &mouse);
		if (!PtInRect(&d->viewport_rect, mouse)) break;
		/* could set mouse x,y pos here if we wanted to */
		/* TODO no x axis scrolling yet */
		/* TODO accumulate error from division */
		SendInputEvent(d, EmuIn_Wheel, 0, 0, (UxnU16)(-zDelta / 120));
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
		case VK_DELETE:  if (!up) { wparam = 127; goto char_down; }

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
		if (wparam < 32 && wparam != 8 && wparam != 9 && wparam != 10 && wparam != 13 && wparam != 27 && wparam != 127) break;
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
			DialogBox(MainInstance, MAKEINTRESOURCE(IDD_ABOUTBOX), hwnd, AboutBoxProc);
			return 0;
		case IDM_EXIT: PostQuitMessage(0); return 0;
		case IDM_OPENROM: OpenROMDialog(d); return 0;
		case IDM_CLONEWINDOW: CloneWindow(d); return 0;
		case IDM_TOGGLEZOOM:
			if (IsZoomed(d->hWnd)) return 0;
			d->viewport_scale = d->viewport_scale == 1 ? 2 : 1;
			RefitEmuWindow(d);
			return 0;
		case IDM_RELOAD: ReloadFromROMFile(d); return 0;
		case IDM_CLOSEWINDOW: PostMessage(hwnd, WM_CLOSE, 0, 0); return 0;
		case IDM_PAUSE: if (d->running) PauseVM(d); else d->box.core.fault = 0, UnpauseVM(d); return 0;
		case IDM_STEP: case IDM_BIGSTEP:
			if (!d->running && d->exec_state)
			{
				OpenBeetbugWindow(d, FALSE);
				SetFocus(d->beetbugHWnd);
				SendMessage(d->beetbugHWnd, msg, wparam, lparam);
			}
			return 0;
		case IDM_TOGGLECONSOLE:
			if (!d->consoleHWnd) CreateConsoleWindow(d);
			ShowWindow(d->consoleHWnd, IsWindowVisible(d->consoleHWnd) ? SW_HIDE : SW_SHOW);
			return 0;
		case IDM_OPENBEETBUG: case IDM_MOREBUG:
			OpenBeetbugWindow(d, LOWORD(wparam) == IDM_MOREBUG);
			return 0;
		case IDM_TOGGLEMENU:
		{
			HMENU tmp = d->hHiddenMenu; LONG scale = d->viewport_scale;
			d->hHiddenMenu = GetMenu(d->hWnd);
			SetMenu(d->hWnd, tmp);
			/* WM_SIZE was sent when calling SetMenu(). If the zoom is 2x, and the menu is unhiding, then CalcUxnViewport() may set viewport_scale to be 1x instead of 2x. Set it back so that the window doesn't shrink. */
			if (!IsZoomed(d->hWnd)) { d->viewport_scale = scale; RefitEmuWindow(d); }
			return 0;
		}
		}
		break;

	case UXNMSG_ContinueExec:
		if (!d->running) return 0;
		if (d->exec_state) RunUxn(d, 0, FALSE); /* Unfinished vector execution */
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
		d->box.core.pc = b->box.core.pc;
		d->box.core.fault = b->box.core.fault;
		d->exec_state = b->exec_state;
		CopyMemory(&d->box.work_stack, &b->box.work_stack, sizeof(UxnStack) * 2);
		CopyMemory(d->box.device_memory, b->box.device_memory, sizeof d->box.device_memory);
		CopyStasher(&d->box, &b->box);
		CopyMemory(d->screen.palette, b->screen.palette, sizeof d->screen.palette);
		SetUxnScreenSize(&d->screen, b->screen.width, b->screen.height);
		d->viewport_scale = b->viewport_scale;
		if (!d->hHiddenMenu != !b->hHiddenMenu) /* Quick hack for now. Do it better later to avoid resize twitch. */
			SendMessage(d->hWnd, WM_COMMAND, MAKEWPARAM(IDM_TOGGLEMENU, 0), 0);
		RefitEmuWindow(d);
		CopyMemory(d->screen.bg, b->screen.bg, d->screen.width * d->screen.height * 2);
		CopyMemory(d->rom_path, b->rom_path, MAX_PATH);
		/* can't copy filer state */
		if (b->running) UnpauseVM(d);
		UpdateWindow(d->hWnd);
		return 0;
	}
	case UXNMSG_SendArgs: if (!CmdLineArgs) break;
	/* Send any additional arguments as virtual console input */
	{
		int arg, i, n; CHAR buff[256];
		for (arg = 0; arg < CmdLineArgCount; arg++)
			if ((n = WideCharToMultiByte(CP_ACP, 0, CmdLineArgs[arg], -1, buff, sizeof buff, NULL, NULL)))
			{
				buff[n - 1] = '\n';
				for (i = 0; i < n; i++) SendInputEvent(d, EmuIn_Console, buff[i], 0, 0);
			}
		return 0;
	}
	case UXNMSG_Screen60hz:
		SendInputEvent(d, EmuIn_Screen, 0, 0, 0);
		return 0;
	}
	return DefWindowProc(hwnd, msg, wparam, lparam);
}

static BOOL SendVBlankMessages(void)
{
	EmuWindow *emu; BOOL result;
	if (WaitForSingleObject(VBlankMutex, 5) != WAIT_OBJECT_0) return FALSE;
	result = !!(emu = ListFront(&emus_needing_vblank, EmuWindow, vblank_link));
	for (; emu; emu = ListNext(emu, EmuWindow, vblank_link))
	{
		if (emu->exec_state == EmuIn_Screen) continue; /* Non-atomic read is OK */
		/* ^ Try not to overwhelm it with 60hz timer events. Otherwise, user input will become unresponsive. */
		PostMessage(emu->hWnd, UXNMSG_Screen60hz, 0, 0);
	}
	ReleaseMutex(VBlankMutex);
	return result;
}
static LONGLONG mm_sand, mm_stamp;
static void CALLBACK EmuMMTimerProc(UINT uTimerID, UINT uMsg, DWORD_PTR dwUser, DWORD_PTR dw1, DWORD_PTR dw2)
{
	LONGLONG delay;
again:
	while (mm_sand - MicrosSince(mm_stamp) >= 500) Sleep(0);
	while (mm_sand - MicrosSince(mm_stamp) >= 0);
	/* if (mm_sand - MicrosSince(mm_stamp) < -10)
		DebugPrint("missed deadline by %d", (int)mm_sand - MicrosSince(mm_stamp)); */
on_resume:
	if (!SendVBlankMessages()) /* Suspend (save energy) if there were no receivers */
	{
		timeEndPeriod(0);
		WaitForSingleObject(ResumeTimerEvent, INFINITE);
		ResetEvent(ResumeTimerEvent);
		timeBeginPeriod(0);
		mm_sand = 0;
		mm_stamp = TimeStampNow();
		goto on_resume;
	}
	mm_sand += 16667 - (LONGLONG)MicrosSince(mm_stamp);
	mm_sand = MAX(mm_sand, 16667 * -3); /* Limit the amount of catchup */
	mm_stamp = TimeStampNow();
	delay = mm_sand / 1000 - 2;
	if (delay < 1) goto again;
	timeSetEvent((UINT)delay, 0, EmuMMTimerProc, 0, TIME_ONESHOT);
	(void)uTimerID, (void)uMsg, (void)dwUser, (void)dw1, (void)dw2;
}

static DWORD WINAPI VBlankThreadProc(void *d)
{
#if ADAPTER_VBLANK
	D3DKMT_OPENADAPTERFROMHDC oadfhdc; DISPLAY_DEVICE ddev;
	int i = 0; BOOL result = 0;
	D3DKMT_WAITFORVERTICALBLANKEVENT wait_e;
	if (!Ptr_D3DKMTWaitForVerticalBlankEvent || !Ptr_D3DKMTOpenAdapterFromHdc || !Ptr_EnumDisplayDevicesA) goto use_mm_timer;
	ZeroMemory(&wait_e, sizeof wait_e);
	ZeroMemory(&ddev, sizeof ddev); ddev.cb = sizeof ddev;
	while (Ptr_EnumDisplayDevicesA(NULL, i++, &ddev, 0) && !(ddev.StateFlags & DISPLAY_DEVICE_PRIMARY_DEVICE));
	if (!(oadfhdc.hDc = CreateDC(NULL, ddev.DeviceName, NULL, NULL))) return FALSE;
	if ((result = Ptr_D3DKMTOpenAdapterFromHdc(&oadfhdc) >= 0))
	{
		wait_e.hAdapter = oadfhdc.hAdapter;
		wait_e.VidPnSourceId = oadfhdc.VidPnSourceId;
	}
	DeleteDC(oadfhdc.hDc);
	if (!result) goto use_mm_timer;;

	for (;;)
	{
		if (Ptr_D3DKMTWaitForVerticalBlankEvent(&wait_e)) goto use_mm_timer;
		if (!SendVBlankMessages()) /* Save energy if there were no receivers */
		{
			WaitForSingleObject(ResumeTimerEvent, INFINITE);
			ResetEvent(ResumeTimerEvent);
		}
	}
use_mm_timer:
#endif
	timeBeginPeriod(0);
	mm_stamp = TimeStampNow();
	EmuMMTimerProc(0, 0, 0, 0, 0);
	return 0; (void)d;
}

int CALLBACK WinMain(HINSTANCE instance, HINSTANCE prev_instance, LPSTR command_line, int show_code)
{
	WNDCLASSEX wc; HWND hWin, hParent;
	MSG msg; HACCEL hAccel; HANDLE hThread; HMODULE hMod;
	DWORD thread_id; static /* <- C89 */ BOOL hide_menu = FALSE;
	Type_CommandLineToArgvW *Ptr_CommandLineToArgvW;
	Type_GetCommandLineW *Ptr_GetCommandLineW;
	EmuWindow *emu;
	(void)command_line; (void)prev_instance;
	QueryPerformanceFrequency(&_perfcount_freq);
	ExecutionTimeLimit = _perfcount_freq.QuadPart / 20;
	MainInstance = instance;
	ProcessHeap = GetProcessHeap();
	emu = HeapAlloc0OrDie(sizeof(EmuWindow));
	CopyMemory(emu->rom_path, DefaultROMPath, (lstrlen(DefaultROMPath) + 1) * sizeof(TCHAR));

#if ADAPTER_VBLANK
	hMod = GetModuleHandle(TEXT("gdi32.dll"));
	Ptr_D3DKMTWaitForVerticalBlankEvent = (Type_D3DKMTWaitForVerticalBlankEvent *)GetProcAddress(hMod, "D3DKMTWaitForVerticalBlankEvent");
	Ptr_D3DKMTOpenAdapterFromHdc = (Type_D3DKMTOpenAdapterFromHdc *)GetProcAddress(hMod, "D3DKMTOpenAdapterFromHdc");
	Ptr_EnumDisplayDevicesA = (Type_EnumDisplayDevicesA *)GetProcAddress(GetModuleHandle(TEXT("user32.dll")), "EnumDisplayDevicesA");
#endif
	(void)hMod;

	/* Prepare any command line args for later use.
	 * Windows 95 won't have the procedures for commandline args, so we'll load them only optionally, at runtime. */
	if ((Ptr_GetCommandLineW = (Type_GetCommandLineW *)GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")), "GetCommandLineW")) &&
		(Ptr_CommandLineToArgvW = (Type_CommandLineToArgvW *)GetProcAddress(GetModuleHandle(TEXT("shell32.dll")), "CommandLineToArgvW")) &&
		(CmdLineArgs = Ptr_CommandLineToArgvW(Ptr_GetCommandLineW(), &CmdLineArgCount)))
	{
		LPCWSTR arg; void const **opt, *options[] ={
			L"hidemenu", &hide_menu,
			L"break", &BreakOnInitVector,
		0};
		while (CmdLineArgs += 1, CmdLineArgCount -= 1)
		{
			if ((arg = *CmdLineArgs, lstrlenW(arg)) < 2 || (arg[0] != L'/' && arg[0] != L'-')) break;
			for (opt = options; opt[0]; opt += 2)
			{
				if (CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, arg + 1, -1, (LPCWSTR)opt[0], -1) != CSTR_EQUAL) continue;
				*(BOOL *)opt[1] = TRUE;
				goto next_arg;
			}
			break; next_arg:;
		}
		if (CmdLineArgCount) /* Next argument will be the ROM file to load, if any */
		{
			if (!WideCharToMultiByte(CP_ACP, 0, CmdLineArgs[0], -1, emu->rom_path, sizeof emu->rom_path, NULL, NULL))
				FatalBox("The command line argument for the file path was too long, or contained characters that couldn't be handled by this program.");
			CmdLineArgs += 1, CmdLineArgCount -= 1;
		}
	}

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
	wc.hIcon = LoadIcon(instance, (LPCTSTR)IDI_BEETBUG);
	RegisterClassEx(&wc);
	hAccel = LoadAccelerators(instance, (LPCSTR)IDC_UXN32);
	InitCommonControls();
	VBlankMutex = CreateMutex(0, FALSE, TEXT("VBlankMutex"));
	ResumeTimerEvent = CreateEvent(0, TRUE, FALSE, TEXT("ResumeTimerEvent"));

	hWin = CreateWindowForEmu(instance, emu);
	if (hide_menu) SendMessage(hWin, WM_COMMAND, MAKEWPARAM(IDM_TOGGLEMENU, 0), 0);
	/* ^ Hacky. Search for other use of IDM_TOGGLEMENU in this file to see other place we do this. When we clean this up, fix it in both places. */
	ShowWindow(hWin, show_code);
	SendMessage(hWin, UXNMSG_SendArgs, 0, 0); /* Send the cmd line args, if any */

	hThread = CreateThread(NULL, 0, VBlankThreadProc, (void *)(SIZE_T)GetCurrentThreadId() /*user data*/, 0, &thread_id);

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
	TerminateThread(hThread, 0);
	return 0;
}
