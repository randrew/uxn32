cmake_minimum_required(VERSION 3.19)
project(uxn LANGUAGES C)

set(CMAKE_C_STANDARD 90)

if ("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang")
    if ("${CMAKE_C_SIMULATE_ID}" STREQUAL "MSVC")
        set(IS_CLANG_CL YES)
    else ()
        set(IS_CLANG YES)
    endif ()
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "MSVC")
    set(IS_MSVC YES)
endif ()

if (IS_MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_NO_CRT_STDIO_INLINE)
    set(MSVC_WARNING_FLAGS
        /Wall   # enable all warnings
        /wd4619 # unknown disabled warning
        /wd4068 # unknown pragmas
        /wd4365 # signed/unsigned mismatch
        /wd4242 # integer narrowing, uxn contains many implicit narrowing conversions
        /wd4267 # more integer narrowing conversions
        /wd4244 # conversion to smaller numeric type (because of int to float)
        /wd5219 # conversion from integer to float or int64 to double
        /wd4820 # padding added to struct
        /wd4061 # enum not explicitly handled by a case label
        /wd4505 # unreferenced local function has been removed
        /wd4514 # unreferenced function has been removed
        /wd4706 # assignment within conditional expression
        /wd4710 # function not inlined
        /wd4711 # function selected for inline expansion
        /wd4774 # format string is not a string literal
        /wd4946 # reinterpret cast used between related types
        /wd4371 # layout of class may have changed from previous version of compiler
        /wd5045 # spectre mitigation would have been used with /Qspectre
        /wd4623 /wd4625 /wd4626 /wd5026 /wd5027 # C++ OOP garbage
        )
    string(REPLACE ";" " " MSVC_WARNING_FLAGS "${MSVC_WARNING_FLAGS}")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${MSVC_WARNING_FLAGS}")
    set_source_files_properties(uxn_core.c PROPERTIES COMPILE_FLAGS /wd4127)

    # Disable incremental linking, which has been buggy for years and often
    # produces corrupt executables and debug info.
    string(REGEX REPLACE "/INCREMENTAL" "" CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG}")
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /INCREMENTAL:NO")
    string(REGEX REPLACE "/INCREMENTAL" "" CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO "${CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO}")
    set(CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO "${CMAKE_EXE_LINKER_FLAGS_RELWITHDEBINFO} /INCREMENTAL:NO")
    string(REGEX REPLACE "/INCREMENTAL" "" CMAKE_EXE_LINKER_FLAGS_RELEASE "${CMAKE_EXE_LINKER_FLAGS_RELEASE}")
    set(CMAKE_EXE_LINKER_FLAGS_RELEASE "${CMAKE_EXE_LINKER_FLAGS_RELEASE} /INCREMENTAL:NO")
endif ()

if (IS_CLANG OR IS_CLANG_CL)
    set(WARN_FLAGS "-Wall -Wpedantic -Wextra -Wwrite-strings -Wconversion -Wshadow -Wunused -Wstrict-prototypes -Wimplicit-fallthrough -Werror=implicit-function-declaration -Werror=implicit-int -Werror=incompatible-pointer-types -Werror=int-conversion -Wno-unknown-pragmas -Wno-shadow-field-in-constructor-modified -Wno-shift-sign-overflow")
    set(NO_WARN_FLAGS "-Wno-switch-enum -Wno-c++98-compat-pedantic -Wno-missing-prototypes -Wno-old-style-cast -Wno-used-but-marked-unused")
    # just for this project
    set(NO_WARN_FLAGS "${NO_WARN_FLAGS} -Wno-implicit-int-float-conversion -Wno-shadow-field-in-constructor -Wno-c++20-compat -Wno-cast-qual -Wno-float-equal -Wno-sign-conversion -Wno-implicit-int-conversion")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${WARN_FLAGS} ${NO_WARN_FLAGS}")
    if (WIN32 AND NOT MINGW) # MinGW doesn't have libubsan or something. No sanitizers work there.
        # asan doesn't seem to work with clang-cl.
        # Maybe it works with clang.exe and lld.exe if you pass the sanitize flags also to lld?
        set(SANITIZERS "-fsanitize=undefined,float-divide-by-zero,implicit-conversion,unsigned-integer-overflow")
    endif ()
endif ()

if (IS_CLANG)
    set(CMAKE_C_FLAGS_DEBUG "-O0 -g")
    set(CMAKE_C_FLAGS_RELEASE "-Oz")
    set(CMAKE_C_FLAGS_RELWITHDEBINFO "-Oz -g")
endif ()

add_executable(Uxn32 WIN32 uxn32.rc uxn_core.c uxn_lz.c uxn32.c)
target_link_libraries(Uxn32 user32.lib gdi32.lib shell32.lib shlwapi.lib comdlg32.lib comctl32.lib winmm.lib)
set_property(TARGET Uxn32 PROPERTY MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:DebugDLL>")
