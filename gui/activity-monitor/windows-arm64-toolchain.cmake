# ============================================================
# Windows ARM64 toolchain for macOS → Windows (clang-cl)
# Qt 6 compatible
# ============================================================

set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_SYSTEM_VERSION 10.0)
set(CMAKE_SYSTEM_PROCESSOR ARM64)

# ------------------------------------------------------------
# Compilers
# ------------------------------------------------------------
set(CMAKE_C_COMPILER clang-cl)
set(CMAKE_CXX_COMPILER clang-cl)
set(CMAKE_LINKER lld-link)

set(CMAKE_C_COMPILER_TARGET arm64-pc-windows-msvc)
set(CMAKE_CXX_COMPILER_TARGET arm64-pc-windows-msvc)

set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

# Pretend to be MSVC (important for Qt)
set(MSVC TRUE)
set(MSVC_VERSION 1930)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
set(WINSDK $ENV{WINSDK})
set(WINSDK_VER $ENV{WINSDK_VER})

set(WINSDK_INC "${WINSDK}/Program Files/Windows Kits/10/Include/${WINSDK_VER}")
set(WINSDK_LIB "${WINSDK}/Program Files/Windows Kits/10/Lib/${WINSDK_VER}")

# xwin CRT layout
set(XWIN_CRT "${WINSDK}/crt")
set(XWIN_SDK "${WINSDK}/sdk")

# ------------------------------------------------------------
# Include directories (ORDER MATTERS)
# ------------------------------------------------------------
include_directories(
  "${XWIN_CRT}/include"
  "${WINSDK_INC}/ucrt"
  "${WINSDK_INC}/shared"
  "${WINSDK_INC}/um"
  "${WINSDK_INC}/winrt"
  "${WINSDK_INC}/cppwinrt"
)

# ------------------------------------------------------------
# Library directories
# ------------------------------------------------------------
link_directories(
  "${XWIN_CRT}/lib/arm64"
  "${WINSDK_LIB}/ucrt/arm64"
  "${WINSDK_LIB}/um/arm64"
)

# ------------------------------------------------------------
# Default Windows system libraries
# ------------------------------------------------------------
set(CMAKE_C_STANDARD_LIBRARIES
  kernel32.lib
  user32.lib
  gdi32.lib
  winspool.lib
  shell32.lib
  ole32.lib
  oleaut32.lib
  uuid.lib
  comdlg32.lib
  advapi32.lib
)

set(CMAKE_CXX_STANDARD_LIBRARIES "${CMAKE_C_STANDARD_LIBRARIES}")

# ------------------------------------------------------------
# Compiler flags
# ------------------------------------------------------------
add_compile_options(
  /nologo
  /EHsc
  /utf-8
  /DWIN32
  /D_WINDOWS
)

# ------------------------------------------------------------
# Linker flags
# ------------------------------------------------------------
set(CMAKE_EXE_LINKER_FLAGS
  "/nologo /SUBSYSTEM:WINDOWS /MANIFEST:NO"
)

set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}")

# ------------------------------------------------------------
# Find behavior
# ------------------------------------------------------------
set(CMAKE_FIND_ROOT_PATH
  "${WINSDK}"
)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)

message(STATUS "Using Windows ARM64 toolchain")
message(STATUS "WINSDK: ${WINSDK}")
message(STATUS "WINSDK_VER: ${WINSDK_VER}")
