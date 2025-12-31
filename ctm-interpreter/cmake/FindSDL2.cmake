# FindSDL2.cmake - Find SDL2 for Pascal bindings
# This module fetches and finds SDL2-for-Pascal bindings
#
# Usage:
#   find_package(SDL2)
#
# Variables defined by this module:
#   SDL2_FOUND              - System has SDL2 for Pascal
#   SDL2_PASCAL_DIR         - SDL2 Pascal bindings directory
#   SDL2_INCLUDE_DIRS       - SDL2 Pascal units directory
#   SDL2_LIBRARIES          - SDL2 native libraries (if found)
#   SDL2_VERSION            - Version of SDL2 (if detectable)
#   SDL2_UNITS              - List of SDL2 Pascal unit files

include(FetchContent)

# =============================================================================
# Configuration
# =============================================================================
set(SDL2_PASCAL_REPO "https://github.com/vic3t3chn0/SDL2-for-Pascal.git")
set(SDL2_TOOLS_DIR "${CMAKE_SOURCE_DIR}/tools")
set(SDL2_DOWNLOAD_DIR "${SDL2_TOOLS_DIR}/SDL2-for-Pascal")
# The directory inside the download where the FPC units are located
set(SDL2_UNITS_DIR "${SDL2_DOWNLOAD_DIR}/units")

message(STATUS "Looking for SDL2 Pascal bindings...")

# =============================================================================
# Fetch SDL2-for-Pascal if not already present
# =============================================================================
if(NOT EXISTS "${SDL2_DOWNLOAD_DIR}")
    message(STATUS "SDL2 Pascal bindings not found, fetching from GitHub...")
    
    FetchContent_Declare(
        SDL2_Pascal
        GIT_REPOSITORY ${SDL2_PASCAL_REPO}
        GIT_TAG main
        GIT_SHALLOW TRUE
        SOURCE_DIR ${SDL2_DOWNLOAD_DIR}
    )
    
    FetchContent_MakeAvailable(SDL2_Pascal)
    
    if(EXISTS "${SDL2_DOWNLOAD_DIR}")
        message(STATUS "SDL2 Pascal bindings downloaded successfully.")
    else()
        message(FATAL_ERROR "Failed to download SDL2 Pascal bindings.")
    endif()
endif()

# =============================================================================
# Check for SDL2 Units and set final variables
# =============================================================================
# Check if the core SDL2 Pascal unit file exists
if(EXISTS "${SDL2_UNITS_DIR}/sdl2.pas")
    set(SDL2_FOUND TRUE)
    set(SDL2_PASCAL_DIR "${SDL2_DOWNLOAD_DIR}")
    set(SDL2_INCLUDE_DIRS "${SDL2_UNITS_DIR}")
    
    message(STATUS "  ✓ SDL2: FOUND")
    message(STATUS "    Units: ${SDL2_INCLUDE_DIRS}")
else()
    set(SDL2_FOUND FALSE)
    message(STATUS "  SDL2: NOT FOUND (sdl2.pas not found in ${SDL2_UNITS_DIR})")
endif()

# =============================================================================
# Handle standard find_package arguments (for REQUIRED/QUIET)
# =============================================================================
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(SDL2
    REQUIRED_VARS SDL2_INCLUDE_DIRS
    FAIL_MESSAGE "Could not find SDL2 Pascal bindings. Check internet connection or repository path."
)

mark_as_advanced(SDL2_PASCAL_DIR SDL2_INCLUDE_DIRS SDL2_LIBRARIES)

# =============================================================================
# Custom Clean Target (to remove downloaded bindings)
# =============================================================================
add_custom_target(clean-sdl2
    COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "Removing downloaded SDL2 bindings from ${SDL2_DOWNLOAD_DIR}..."
    COMMAND ${CMAKE_COMMAND} -E remove_directory "${SDL2_DOWNLOAD_DIR}"
    COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --green "SDL2 bindings removed. Re-run CMake to re-download."
    COMMENT "Run 'make clean-sdl2' or 'ninja clean-sdl2' to force re-download of SDL2 Pascal bindings."
)