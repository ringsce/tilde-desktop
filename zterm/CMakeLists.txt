cmake_minimum_required(VERSION 3.20)
project(zterminal C CXX)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# macOS specific settings
if(APPLE)
    set(CMAKE_OSX_ARCHITECTURES "arm64")
    find_library(COCOA_LIBRARY Cocoa REQUIRED)
    find_library(CORE_FOUNDATION CoreFoundation REQUIRED)
    find_library(APPKIT_LIBRARY AppKit REQUIRED)
endif()

# Header files
set(HEADERS
    src/terminal.h
    src/pty.h
    src/tabs.h
    src/mouse.h
    src/clipboard.h
    src/config.h
    src/scrollback.h
    src/search.h
)

add_executable(zterminal
        src/main.c
        src/terminal.c
        src/pty.c
        src/tabs.c
        src/mouse.c
        src/clipboard.m
        src/tabs.c
        src/main.c
        src/pty.c
        src/search.c
        src/scrollback.c
        src/config.c
        README.md
)

target_include_directories(zterminal PRIVATE src)

if(APPLE)
    target_link_libraries(zterminal
            ${COCOA_LIBRARY}
            ${CORE_FOUNDATION}
            ${APPKIT_LIBRARY}
    )
endif()

# Enable warnings
target_compile_options(zterminal PRIVATE -Wall -Wextra -pedantic)

# Enable Objective-C for clipboard.m
set_source_files_properties(src/clipboard.m PROPERTIES COMPILE_FLAGS "-x objective-c")
