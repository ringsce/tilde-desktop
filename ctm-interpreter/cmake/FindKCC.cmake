# FindKCC.cmake - Find KCC (Koran C Compiler)
# This module fetches the KCC repository from GitHub.
#
# Usage:
#   find_package(KCC)
#
# Variables defined by this module:
#   KCC_FOUND          : System has KCC files
#   KCC_DIR            : Directory where KCC was cloned

include(FetchContent)

# =============================================================================
# Configuration
# =============================================================================
set(KCC_REPO "https://github.com/ringsce/kcc.git")
set(TOOLS_DIR "${CMAKE_SOURCE_DIR}/tools")
set(KCC_DOWNLOAD_DIR "${TOOLS_DIR}/kcc")

message(STATUS "Looking for KCC in ${KCC_DOWNLOAD_DIR}...")

# =============================================================================
# Fetch KCC if not already present
# =============================================================================
if(NOT EXISTS "${KCC_DOWNLOAD_DIR}/.git")
    message(STATUS "KCC not found, fetching from GitHub into 'tools/kcc' directory...")
    
    FetchContent_Declare(
        KCC_Repo
        GIT_REPOSITORY ${KCC_REPO}
        GIT_TAG main
        GIT_SHALLOW TRUE
        SOURCE_DIR ${KCC_DOWNLOAD_DIR}
    )
    
    FetchContent_MakeAvailable(KCC_Repo)
    
    if(EXISTS "${KCC_DOWNLOAD_DIR}")
        set(KCC_FOUND TRUE)
        set(KCC_DIR "${KCC_DOWNLOAD_DIR}")
        message(STATUS "KCC downloaded successfully to: ${KCC_DIR}")
    else()
        set(KCC_FOUND FALSE)
        message(FATAL_ERROR "Failed to download KCC")
    endif()
else()
    set(KCC_FOUND TRUE)
    set(KCC_DIR "${KCC_DOWNLOAD_DIR}")
    message(STATUS "KCC found at: ${KCC_DIR}")
endif()

# =============================================================================
# Handle standard find_package arguments
# =============================================================================
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(KCC
    REQUIRED_VARS KCC_DIR
    FAIL_MESSAGE "Could not find KCC. Check your internet connection."
)

mark_as_advanced(KCC_DIR)

# =============================================================================
# Custom Clean Target (to remove downloaded KCC)
# =============================================================================
add_custom_target(clean-kcc
    COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "Removing downloaded KCC from ${KCC_DOWNLOAD_DIR}..."
    COMMAND ${CMAKE_COMMAND} -E remove_directory "${KCC_DOWNLOAD_DIR}"
    COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --green "KCC removed. Re-run CMake to re-download."
    COMMENT "Run 'make clean-kcc' or 'ninja clean-kcc' to force re-download of KCC."
)
