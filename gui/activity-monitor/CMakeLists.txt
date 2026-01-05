cmake_minimum_required(VERSION 3.16)
project(ActivityMonitor C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Detect platform and architecture
message(STATUS "=== Platform Detection ===")
message(STATUS "System: ${CMAKE_SYSTEM_NAME}")
message(STATUS "Processor: ${CMAKE_SYSTEM_PROCESSOR}")
message(STATUS "Host System: ${CMAKE_HOST_SYSTEM_NAME}")

# Set platform-specific variables
set(IS_WINDOWS FALSE)
set(IS_MACOS FALSE)
set(IS_LINUX FALSE)
set(IS_ARM64 FALSE)
set(IS_X86_64 FALSE)

# Detect OS
if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    set(IS_WINDOWS TRUE)
    message(STATUS "Building for: Windows")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set(IS_MACOS TRUE)
    message(STATUS "Building for: macOS")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(IS_LINUX TRUE)
    message(STATUS "Building for: Linux")
endif()

# Detect Architecture
if(CMAKE_SYSTEM_PROCESSOR MATCHES "^(aarch64|ARM64|arm64)$")
    set(IS_ARM64 TRUE)
    message(STATUS "Architecture: ARM64")
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^(x86_64|AMD64|amd64)$")
    set(IS_X86_64 TRUE)
    message(STATUS "Architecture: x86_64")
else()
    message(STATUS "Architecture: ${CMAKE_SYSTEM_PROCESSOR}")
endif()

# Platform-specific configuration
if(IS_WINDOWS)
    message(STATUS "Configuring for Windows ARM64")
    add_compile_definitions(WINDOWS_ARM64)
elseif(IS_MACOS AND IS_ARM64)
    message(STATUS "Configuring for macOS Apple Silicon")
    add_compile_definitions(MACOS_ARM64)
elseif(IS_MACOS)
    message(STATUS "Configuring for macOS Intel")
    add_compile_definitions(MACOS_X86_64)
elseif(IS_LINUX AND IS_ARM64)
    message(STATUS "Configuring for Linux ARM64")
    add_compile_definitions(LINUX_ARM64)
elseif(IS_LINUX)
    message(STATUS "Configuring for Linux x86_64")
    add_compile_definitions(LINUX_X86_64)
endif()

message(STATUS "=========================")

# PHNT (Process Hacker Native API headers) - Windows only
if(IS_WINDOWS)
    option(USE_PHNT "Use Process Hacker Native API headers" ON)
    if(USE_PHNT)
        if(EXISTS "${CMAKE_SOURCE_DIR}/phnt")
            message(STATUS "Using PHNT headers from: ${CMAKE_SOURCE_DIR}/phnt")
            include_directories(${CMAKE_SOURCE_DIR}/phnt)
            add_compile_definitions(PHNT_MODE=PHNT_MODE_KERNEL)
            add_compile_definitions(PHNT_VERSION=PHNT_WIN11)
        else()
            message(WARNING "PHNT not found at ${CMAKE_SOURCE_DIR}/phnt")
            message(STATUS "Clone it with: git clone https://github.com/processhacker/phnt.git")
        endif()
    endif()
endif()

# Try to find Qt6
find_package(Qt6 COMPONENTS Core Gui Widgets QUIET)

if(Qt6_FOUND)
    message(STATUS "Qt6 found - building full Qt application")
    set(CMAKE_AUTOMOC ON)
    set(CMAKE_AUTORCC ON)
    set(CMAKE_AUTOUIC ON)
    
    # Qt-based application
    if(IS_WINDOWS)
        add_executable(ActivityMonitor WIN32 main.cpp)
    else()
        add_executable(ActivityMonitor main.cpp)
    endif()
    
    target_link_libraries(ActivityMonitor
        Qt6::Core
        Qt6::Gui
        Qt6::Widgets
    )
    
    # Platform-specific libraries
    if(IS_WINDOWS)
        target_link_libraries(ActivityMonitor
            kernel32
            user32
            advapi32
            ntdll
        )
    elseif(IS_MACOS)
        find_library(COCOA_FRAMEWORK Cocoa)
        find_library(IOKIT_FRAMEWORK IOKit)
        target_link_libraries(ActivityMonitor
            ${COCOA_FRAMEWORK}
            ${IOKIT_FRAMEWORK}
        )
    elseif(IS_LINUX)
        target_link_libraries(ActivityMonitor
            pthread
            dl
        )
    endif()
else()
    message(WARNING "Qt6 not found - building simple test program")
    message(STATUS "To build with Qt6, set Qt6_DIR or install Qt6 for your platform")
    
    # Simple test program without Qt6
    add_executable(ActivityMonitor main.cpp)
    
    # Platform-specific libraries
    if(IS_WINDOWS)
        target_link_libraries(ActivityMonitor
            kernel32
            user32
            advapi32
            ntdll
        )
    elseif(IS_LINUX)
        target_link_libraries(ActivityMonitor
            pthread
        )
    endif()
    # macOS doesn't need explicit libraries for simple programs
endif()

message(STATUS "Build configured for ${CMAKE_SYSTEM_NAME} ${CMAKE_SYSTEM_PROCESSOR}")
message(STATUS "Compiler: ${CMAKE_CXX_COMPILER}")
message(STATUS "Target: ${CMAKE_SYSTEM_PROCESSOR}")