@echo off
if not exist uxn32.c cd ..
if not exist uxn32.c echo Run this with the uxn32 root source directory as your working directory & pause & exit /b 1
@echo on

if not exist build mkdir build

set INCLUDE=C:\Program Files\PellesC\Include\Win;C:\Program Files\PellesC\Include
set LIB=C:\Program Files\PellesC\Lib\Win;C:\Program Files\PellesC\Lib

"C:\Program Files\PellesC\Bin\pocc.exe" -std:C99 -Tx86-coff -MT -Os -Ox -Ob1 -fp:precise -W0 -Gz -Ze -DNDEBUG -DWIN32 -D_WINDOWS -D_MBCS -D_WIN32_WINNT=0x0400 "uxn_core.c" -Fo"build\uxn_core.obj"
"C:\Program Files\PellesC\Bin\pocc.exe" -std:C99 -Tx86-coff -MT -Os -Ox -Ob1 -fp:precise -W0 -Gz -Ze -DNDEBUG -DWIN32 -D_WINDOWS -D_MBCS -D_WIN32_WINNT=0x0400 "uxn_lz.c" -Fo"build\uxn_lz.obj"
"C:\Program Files\PellesC\Bin\pocc.exe" -std:C99 -Tx86-coff -MT -Os -Ox -Ob1 -fp:precise -W0 -Gz -Ze -DNDEBUG -DWIN32 -D_WINDOWS -D_MBCS -D_WIN32_WINNT=0x0400 "uxn32.c" -Fo"build\uxn32.obj"
"C:\Program Files\PellesC\Bin\porc.exe" -L0x409 -DNDEBUG "uxn32.rc" -Fo"build\uxn32.res"
"C:\Program Files\PellesC\Bin\polink.exe" -subsystem:windows -machine:x86 user32.lib gdi32.lib shell32.lib shlwapi.lib comdlg32.lib comctl32.lib winmm.lib -out:"build\Uxn32.exe" "build\uxn_core.obj" "build\uxn_lz.obj" "build\uxn32.obj" "build\uxn32.res"
