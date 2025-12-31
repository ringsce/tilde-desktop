# FindKayteIDE.cmake - Find KayteIDE for Ekron Realms
# This module fetches the KayteIDE repository from GitHub.
#
# Usage:
#   find_package(KayteIDE)
#
# Variables defined by this module:
#   KAYTEIDE_FOUND          : System has KayteIDE files
#   KAYTEIDE_DIR            : Directory where KayteIDE was cloned

include(FetchContent)

# =============================================================================
# Configuration
# =============================================================================
set(KAYTEIDE_REPO "https://github.com/ringsce/kayteide.git")
# Target directory is 'ide' in the root project directory (${CMAKE_SOURCE_DIR})
set(KAYTEIDE_DOWNLOAD_DIR "${CMAKE_SOURCE_DIR}/ide")

message(STATUS "Looking for KayteIDE in the project root...")

# =============================================================================
# Fetch KayteIDE if not already present
# =============================================================================
if(NOT EXISTS "${KAYTEIDE_DOWNLOAD_DIR}/.git")
    message(STATUS "KayteIDE not found, fetching from GitHub into 'ide' directory...")
    
    FetchContent_Declare(
        KayteIDE_Repo
        GIT_REPOSITORY ${KAYTEIDE_REPO}
        GIT_TAG main
        GIT_SHALLOW TRUE
        SOURCE_DIR ${KAYTEIDE_DOWNLOAD_DIR}
    )
    
    FetchContent_MakeAvailable(KayteIDE_Repo)
    
    if(EXISTS "${KAYTEIDE_DOWNLOAD_DIR}")
        set(KAYTEIDE_FOUND TRUE)
        set(KAYTEIDE_DIR "${KAYTEIDE_DOWNLOAD_DIR}")
        message(STATUS "KayteIDE downloaded successfully to: ${KAYTEIDE_DIR}")
    else()
        set(KAYTEIDE_FOUND FALSE)
        message(FATAL_ERROR "Failed to download KayteIDE")
    endif()
else()
    set(KAYTEIDE_FOUND TRUE)
    set(KAYTEIDE_DIR "${KAYTEIDE_DOWNLOAD_DIR}")
    message(STATUS "KayteIDE found at: ${KAYTEIDE_DIR}")
endif()

# =============================================================================
# Handle standard find_package arguments
# =============================================================================
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(KayteIDE
    REQUIRED_VARS KAYTEIDE_DIR
    FAIL_MESSAGE "Could not find KayteIDE. Check your internet connection."
)

mark_as_advanced(KAYTEIDE_DIR)

# =============================================================================
# Custom Clean Target (to remove downloaded IDE)
# =============================================================================
# This allows the user to easily remove the directory and force a re-download.

add_custom_target(clean-kayteide
    COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "Removing downloaded KayteIDE from ${KAYTEIDE_DOWNLOAD_DIR}..."
    COMMAND ${CMAKE_COMMAND} -E remove_directory "${KAYTEIDE_DOWNLOAD_DIR}"
    COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --green "KayteIDE removed. Re-run CMake to re-download."
    COMMENT "Run 'make clean-kayteide' or 'ninja clean-kayteide' to force re-download of KayteIDE."
)