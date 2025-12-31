# FindSDL3.cmake - Find SDL3 for Pascal bindings
# This module fetches and finds SDL3-for-Pascal bindings
#
# Usage:
#   find_package(SDL3)
#
# Variables defined by this module:
#   SDL3_FOUND              - System has SDL3 for Pascal
#   SDL3_PASCAL_DIR         - SDL3 Pascal bindings directory
#   SDL3_INCLUDE_DIRS       - SDL3 Pascal units directory
#   SDL3_LIBRARIES          - SDL3 native libraries (if found)
#   SDL3_VERSION            - Version of SDL3 (if detectable)
#   SDL3_UNITS              - List of SDL3 Pascal unit files

include(FetchContent)

# =============================================================================
# Configuration
# =============================================================================
set(SDL3_PASCAL_REPO "https://github.com/vic3t3chn0/SDL3-for-Pascal.git")
set(SDL3_TOOLS_DIR "${CMAKE_SOURCE_DIR}/tools")
set(SDL3_DOWNLOAD_DIR "${SDL3_TOOLS_DIR}/SDL3-for-Pascal")

message(STATUS "Looking for SDL3 Pascal bindings...")

# =============================================================================
# Fetch SDL3-for-Pascal if not already present
# =============================================================================
if(NOT EXISTS "${SDL3_DOWNLOAD_DIR}")
    message(STATUS "SDL3 Pascal bindings not found, fetching from GitHub...")
    
    FetchContent_Declare(
        SDL3_Pascal
        GIT_REPOSITORY ${SDL3_PASCAL_REPO}
        GIT_TAG main
        GIT_SHALLOW TRUE
        SOURCE_DIR ${SDL3_DOWNLOAD_DIR}
    )
    
    FetchContent_MakeAvailable(SDL3_Pascal)
    
    if(EXISTS "${SDL3_DOWNLOAD_DIR}")
        message(STATUS "SDL3 Pascal bindings downloaded successfully")
    else()
        message(FATAL_ERROR "Failed to download SDL3 Pascal bindings")
    endif()
else()
    message(STATUS "SDL3 Pascal bindings found at: ${SDL3_DOWNLOAD_DIR}")
endif()

# =============================================================================
# Detect SDL3 Pascal bindings structure
# =============================================================================
set(SDL3_PASCAL_DIR "${SDL3_DOWNLOAD_DIR}")

# Find units directory - check multiple possible locations
set(POSSIBLE_UNIT_DIRS
    "${SDL3_PASCAL_DIR}/units"
    "${SDL3_PASCAL_DIR}/SDL3"
    "${SDL3_PASCAL_DIR}/src"
    "${SDL3_PASCAL_DIR}"
)

foreach(dir ${POSSIBLE_UNIT_DIRS})
    if(EXISTS "${dir}/sdl3.pas" OR EXISTS "${dir}/SDL3.pas")
        set(SDL3_INCLUDE_DIRS "${dir}")
        break()
    endif()
endforeach()

# If still not found, search recursively
if(NOT SDL3_INCLUDE_DIRS)
    file(GLOB_RECURSE SDL3_UNIT_FILE "${SDL3_PASCAL_DIR}/sdl3.pas" "${SDL3_PASCAL_DIR}/SDL3.pas")
    if(SDL3_UNIT_FILE)
        list(GET SDL3_UNIT_FILE 0 FIRST_UNIT)
        get_filename_component(SDL3_INCLUDE_DIRS "${FIRST_UNIT}" DIRECTORY)
    endif()
endif()

# =============================================================================
# Find SDL3 Pascal unit files
# =============================================================================
if(SDL3_INCLUDE_DIRS)
    file(GLOB SDL3_UNITS 
        "${SDL3_INCLUDE_DIRS}/*.pas"
        "${SDL3_INCLUDE_DIRS}/*.pp"
    )
    
    list(LENGTH SDL3_UNITS SDL3_UNITS_COUNT)
    message(STATUS "Found ${SDL3_UNITS_COUNT} SDL3 Pascal units in ${SDL3_INCLUDE_DIRS}")
endif()

# =============================================================================
# Find native SDL3 libraries (optional, for linking info)
# =============================================================================
if(WIN32)
    # Windows
    find_library(SDL3_LIBRARY
        NAMES SDL3 sdl3
        PATHS
            "${SDL3_PASCAL_DIR}/lib/win32"
            "${SDL3_PASCAL_DIR}/lib/win64"
            "C:/SDL3/lib"
        PATH_SUFFIXES lib bin
    )
    find_file(SDL3_DLL NAMES SDL3.dll)
    
elseif(APPLE)
    # macOS
    find_library(SDL3_LIBRARY
        NAMES SDL3
        PATHS
            "${SDL3_PASCAL_DIR}/lib/macos"
            /Library/Frameworks
            /usr/local/lib
            /opt/homebrew/lib
        PATH_SUFFIXES lib
    )
    
else()
    # Linux
    find_library(SDL3_LIBRARY
        NAMES SDL3 sdl3
        PATHS
            "${SDL3_PASCAL_DIR}/lib/linux"
            /usr/lib
            /usr/local/lib
            /usr/lib/x86_64-linux-gnu
            /usr/lib/aarch64-linux-gnu
        PATH_SUFFIXES lib lib64
    )
endif()

if(SDL3_LIBRARY)
    set(SDL3_LIBRARIES ${SDL3_LIBRARY})
    message(STATUS "Found SDL3 native library: ${SDL3_LIBRARY}")
else()
    message(STATUS "SDL3 native library not found (Pascal bindings will use dynamic loading)")
endif()

# =============================================================================
# Try to detect SDL3 version (basic check)
# =============================================================================
if(SDL3_INCLUDE_DIRS)
    if(EXISTS "${SDL3_INCLUDE_DIRS}/sdl3.pas")
    file(STRINGS "${SDL3_INCLUDE_DIRS}/sdl3.pas" SDL3_VERSION_LINE
            REGEX "SDL_MAJOR_VERSION.*=.*[0-9]"
            LIMIT_COUNT 1
        )
        # (This logic can be expanded just like in the SDL2 file)
    endif()
endif()

# =============================================================================
# Handle standard find_package arguments
# =============================================================================
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(SDL3
    REQUIRED_VARS SDL3_PASCAL_DIR SDL3_INCLUDE_DIRS
    VERSION_VAR SDL3_VERSION
    FAIL_MESSAGE "Could not find SDL3 Pascal bindings. Check your internet connection."
)

mark_as_advanced(
    SDL3_PASCAL_DIR
    SDL3_INCLUDE_DIRS
    SDL3_LIBRARY
)

# =============================================================================
# Print information if found
# =============================================================================
if(SDL3_FOUND)
    message(STATUS "")
    message(STATUS "=== SDL3 for Pascal ===")
    message(STATUS "  Bindings directory: ${SDL3_PASCAL_DIR}")
    message(STATUS "  Units directory: ${SDL3_INCLUDE_DIRS}")
    message(STATUS "  Number of units: ${SDL3_UNITS_COUNT}")
    if(SDL3_VERSION)
        message(STATUS "  Version: ${SDL3_VERSION}")
    endif()
    if(SDL3_LIBRARIES)
        message(STATUS "  Native library: ${SDL3_LIBRARIES}")
    endif()
    if(SDL3_DLL)
        message(STATUS "  SDL3 DLL: ${SDL3_DLL}")
    endif()
    message(STATUS "")
endif()

# =============================================================================
# Helper function to add SDL3 to FPC projects
# =============================================================================
function(target_link_sdl3 target_name)
    if(NOT SDL3_FOUND)
        message(FATAL_ERROR "SDL3 not found. Cannot link to target ${target_name}")
    endif()
    
    set_property(TARGET ${target_name}
        APPEND PROPERTY SDL3_UNITS_PATH "${SDL3_INCLUDE_DIRS}"
    )
    
    message(STATUS "Target ${target_name} will use SDL3 Pascal bindings")
endfunction()

# =F===========================================================================
# Platform-specific installation hints
# =============================================================================
if(SDL3_FOUND AND NOT SDL3_LIBRARIES)
    message(STATUS "")
    message(STATUS "=== SDL3 Runtime Installation ===")
    message(STATUS "  The SDL3 native library was not found.")
    message(STATUS "  Download it from: https://github.com/libsdl-org/SDL/releases")
    if(WIN32)
        message(STATUS "  Place SDL3.dll next to your executable.")
    elseif(APPLE)
        message(STATUS "  Install SDL3.framework or libSDL3.dylib (e.g., 'brew install sdl3')")
    else()
        message(STATUS "  Install libSDL3.so (e.g., 'sudo apt install libsdl3-dev')")
    endif()
    message(STATUS "")
endif()

# =============================================================================
# Custom Clean Target (to remove downloaded bindings)
# =============================================================================
add_custom_target(clean-sdl3
    COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "Removing downloaded SDL3 bindings from ${SDL3_DOWNLOAD_DIL}..."
    COMMAND ${CMAKE_COMMAND} -E remove_directory "${SDL3_DOWNLOAD_DIR}"
    COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --green "SDL3 bindings removed. Re-run CMake to re-download."
    COMMENT "Run 'make clean-sdl3' or 'ninja clean-sdl3' to force re-download of SDL3 bindings."
)