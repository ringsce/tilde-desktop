# Qt6 cross-compilation toolchain for Windows ARM64
# Save as qt6-cross-toolchain.cmake and use with Qt6 configure:
# cmake -DCMAKE_TOOLCHAIN_FILE=qt6-cross-toolchain.cmake ...

set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_SYSTEM_PROCESSOR ARM64)

# Compilers
set(CMAKE_C_COMPILER /usr/local/llvm/bin/clang-cl)
set(CMAKE_CXX_COMPILER /usr/local/llvm/bin/clang-cl)
set(CMAKE_LINKER /usr/local/llvm/bin/lld-link)
set(CMAKE_RC_COMPILER /usr/local/llvm/bin/llvm-rc)
set(CMAKE_MT /usr/local/llvm/bin/llvm-mt)

# Target triple
set(CMAKE_C_COMPILER_TARGET arm64-pc-windows-msvc)
set(CMAKE_CXX_COMPILER_TARGET arm64-pc-windows-msvc)

# Skip compiler checks
set(CMAKE_C_COMPILER_WORKS TRUE)
set(CMAKE_CXX_COMPILER_WORKS TRUE)

# Windows SDK paths
set(WINSDK_BASE "/Users/pedro/winsdk-arm64")
set(WINSDK_VER "10.0.26100.0")

# Detect SDK structure
if(EXISTS "${WINSDK_BASE}/Program Files/Windows Kits/10")
    set(WINSDK "${WINSDK_BASE}/Program Files/Windows Kits/10")
    set(MSVC_BASE "${WINSDK_BASE}/Program Files/Microsoft Visual Studio")
else()
    set(WINSDK "${WINSDK_BASE}/sdk")
    set(MSVC_BASE "${WINSDK_BASE}/crt")
endif()

# Include directories
include_directories(SYSTEM
    "${WINSDK}/Include/${WINSDK_VER}/ucrt"
    "${WINSDK}/Include/${WINSDK_VER}/um"
    "${WINSDK}/Include/${WINSDK_VER}/shared"
)

# Find MSVC CRT
file(GLOB MSVC_VERSIONS "${MSVC_BASE}/include/*")
if(MSVC_VERSIONS)
    list(GET MSVC_VERSIONS 0 MSVC_VER_DIR)
    get_filename_component(MSVC_VER ${MSVC_VER_DIR} NAME)
    include_directories(SYSTEM "${MSVC_BASE}/include/${MSVC_VER}")
endif()

# Library directories
link_directories(
    "${WINSDK}/Lib/${WINSDK_VER}/ucrt/arm64"
    "${WINSDK}/Lib/${WINSDK_VER}/um/arm64"
)

if(EXISTS "${MSVC_BASE}/lib/arm64")
    link_directories("${MSVC_BASE}/lib/arm64")
endif()

# Linker flags
set(CMAKE_EXE_LINKER_FLAGS_INIT "/machine:ARM64 /subsystem:console")
set(CMAKE_SHARED_LINKER_FLAGS_INIT "/machine:ARM64")

# Don't search for programs in build host
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
