Uxn32 - Uxn Emulator for Win32
==============================

![](https://raw.githubusercontent.com/wiki/randrew/uxn32/uxn32-banner.png)

Uxn32 is a graphical emulator for the [Uxn](https://wiki.xxiivv.com/site/uxn.html) virtual machine.

Features
--------

* Compatible with: Windows NT4, Windows 2000, Windows XP, Windows Vista, Windows 7, Windows 8, Windows 8.1, Windows 10, Windows 11, and Wine. Works on both 32-bit and 64-bit systems.
* Specifically built to also work with Wine on Linux and other OSs.
* Small and simple - single .exe, no installer, no dependencies.
* Sandboxed file system access for Uxn program ROMs.
* Uxn programs receive events at full speed and repaint immediately. Got a 240hz display? Now your Uxn drawing program can make use of it.
* Pre-emptive execution, so it won't freeze up if the Uxn program goes into an infinite loop, unlike the SDL2 version.
* Compiles with: every version of Visual Studio back to and including VC6, Clang, clang-cl, MinGW clang, and probably GCC if you change a couple of lines.

Use
---

Download the pre-built [Uxn Starter Pack - a bundle of Uxn32.exe and a collection of ROMs](https://github.com/randrew/uxn32/releases/download/1.0/Uxn32-1.0-Plus-Start-Pack-ROMs.zip). Unzip into a new directory and run Uxn32.exe.

Uxn32 TODO
----------

- Add drag'n'drop ROM loading.
- Audio output.
- Add console input and output.
- Add File, View, etc. menus.
- Add preferences.
- Add a graphical debugger.
- Add "About" dialog box. (Currently sitting unused in the resources file.)
- Add a preference to set filesystem sandbox directory, and to restrict read and write operations. (-esystem sandboxing is currently based on the current working directory.)
- Fix missing and inconsistent 'static' on functions.
- Fix not redrawing the empty area around the Uxn virtual screen in the window client area when -tied [ ]ue to external reasons (like dragging a window over it in a non-composited Windows desktop.)
- Use a better timing and display sync method. (Might require special casing for composited Windows [ -ktops.)
- Test and fix C89 issues with GCC in MinGW, if any. (Tested and works in VC6, MSVC 2019, clang-cl, [ -GW-w64 Clang, but not yet tested in GCC.)
- Reorganize some Device stuff.
- Add a resource usage and performance indicator panel. (See how much % Uxn virtual CPU time is being used, time spent on file access, etc.)

License
-------

ISC
