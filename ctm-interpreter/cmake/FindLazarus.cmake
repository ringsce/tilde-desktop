# FindLazarus.cmake - Find Lazarus IDE and its components
# This module finds the Lazarus IDE installation
#
# Usage:
#   find_package(Lazarus)
#
# Variables defined by this module:
#   LAZARUS_FOUND              - System has Lazarus IDE
#   LAZARUS_ROOT_DIR           - Root directory of Lazarus installation
#   LAZARUS_EXECUTABLE         - The lazarus IDE executable
#   LAZBUILD_EXECUTABLE        - The lazbuild command-line builder
#   LAZARUS_VERSION            - The version of Lazarus found
#   LAZARUS_VERSION_MAJOR      - Major version number
#   LAZARUS_VERSION_MINOR      - Minor version number
#   LAZARUS_VERSION_PATCH      - Patch version number
#   LAZARUS_LCL_DIR            - LCL (Lazarus Component Library) directory
#   LAZARUS_COMPONENTS_DIR     - Components directory
#   LAZARUS_PACKAGER_DIR       - Packager directory
#   LAZARUS_DEBUGGER_DIR       - Debugger directory
#   LAZARUS_UNITS_DIR          - Compiled units directory
#   FPC_FROM_LAZARUS           - FPC executable bundled with Lazarus

# Define common search paths for different platforms
set(LAZARUS_SEARCH_PATHS "")

if(WIN32)
    # Windows 11/10 paths
    list(APPEND LAZARUS_SEARCH_PATHS
        "C:/lazarus"
        "C:/Program Files/Lazarus"
        "C:/Program Files (x86)/Lazarus"
        "$ENV{ProgramFiles}/Lazarus"
        "$ENV{ProgramFiles\(x86\)}/Lazarus"
        "$ENV{LOCALAPPDATA}/lazarus"
        "D:/lazarus"
        "E:/lazarus"
    )
elseif(APPLE)
    # macOS paths (Intel and Apple Silicon)
    list(APPEND LAZARUS_SEARCH_PATHS
        "/Applications/Lazarus.app/Contents/MacOS"
        "/Applications/Lazarus"
        "/usr/local/share/lazarus"
        "/usr/local/lazarus"
        "/opt/lazarus"
        "/opt/local/share/lazarus"
        # Homebrew paths
        "/opt/homebrew/share/lazarus"
        "/usr/local/Cellar/lazarus"
        "/opt/homebrew/Cellar/lazarus"
        # User installations
        "$ENV{HOME}/lazarus"
        "$ENV{HOME}/Applications/Lazarus.app/Contents/MacOS"
    )
else()
    # Linux paths
    list(APPEND LAZARUS_SEARCH_PATHS
        "/usr/lib/lazarus"
        "/usr/lib64/lazarus"
        "/usr/share/lazarus"
        "/usr/local/lib/lazarus"
        "/usr/local/share/lazarus"
        "/opt/lazarus"
        # Snap installations
        "/snap/lazarus/current"
        # User installations
        "$ENV{HOME}/lazarus"
        "$ENV{HOME}/.lazarus"
        # Distribution-specific paths
        "/usr/lib/x86_64-linux-gnu/lazarus"
        "/usr/lib/i386-linux-gnu/lazarus"
        "/usr/lib/aarch64-linux-gnu/lazarus"
    )
endif()

# Find Lazarus root directory by looking for lazbuild first
find_program(LAZBUILD_EXECUTABLE
    NAMES lazbuild lazbuild.exe
    PATHS ${LAZARUS_SEARCH_PATHS}
    PATH_SUFFIXES bin
    DOC "Lazarus command-line builder"
)

# If lazbuild is found, determine the root directory
if(LAZBUILD_EXECUTABLE)
    get_filename_component(LAZBUILD_DIR ${LAZBUILD_EXECUTABLE} DIRECTORY)
    get_filename_component(LAZARUS_ROOT_DIR ${LAZBUILD_DIR} DIRECTORY)
    
    # On some systems, lazbuild might be directly in the root
    if(NOT EXISTS "${LAZARUS_ROOT_DIR}/lcl")
        set(LAZARUS_ROOT_DIR ${LAZBUILD_DIR})
    endif()
endif()

# Find Lazarus IDE executable
if(WIN32)
    set(LAZARUS_EXEC_NAMES lazarus.exe startlazarus.exe)
elseif(APPLE)
    set(LAZARUS_EXEC_NAMES lazarus startlazarus)
else()
    set(LAZARUS_EXEC_NAMES lazarus startlazarus)
endif()

find_program(LAZARUS_EXECUTABLE
    NAMES ${LAZARUS_EXEC_NAMES}
    PATHS ${LAZARUS_SEARCH_PATHS}
    PATH_SUFFIXES bin
    DOC "Lazarus IDE executable"
)

# Try to find root dir from IDE executable if lazbuild wasn't found
if(LAZARUS_EXECUTABLE AND NOT LAZARUS_ROOT_DIR)
    get_filename_component(LAZARUS_BIN_DIR ${LAZARUS_EXECUTABLE} DIRECTORY)
    get_filename_component(LAZARUS_ROOT_DIR ${LAZARUS_BIN_DIR} DIRECTORY)
    
    if(NOT EXISTS "${LAZARUS_ROOT_DIR}/lcl")
        set(LAZARUS_ROOT_DIR ${LAZARUS_BIN_DIR})
    endif()
endif()

# Detect Lazarus version
if(LAZARUS_ROOT_DIR AND EXISTS "${LAZARUS_ROOT_DIR}/version.txt")
    file(READ "${LAZARUS_ROOT_DIR}/version.txt" LAZARUS_VERSION_FILE)
    string(STRIP "${LAZARUS_VERSION_FILE}" LAZARUS_VERSION)
    
    # Parse version components
    if(LAZARUS_VERSION MATCHES "^([0-9]+)\\.([0-9]+)\\.([0-9]+)")
        set(LAZARUS_VERSION_MAJOR ${CMAKE_MATCH_1})
        set(LAZARUS_VERSION_MINOR ${CMAKE_MATCH_2})
        set(LAZARUS_VERSION_PATCH ${CMAKE_MATCH_3})
    elseif(LAZARUS_VERSION MATCHES "^([0-9]+)\\.([0-9]+)")
        set(LAZARUS_VERSION_MAJOR ${CMAKE_MATCH_1})
        set(LAZARUS_VERSION_MINOR ${CMAKE_MATCH_2})
        set(LAZARUS_VERSION_PATCH 0)
    endif()
endif()

# Set important Lazarus directories
if(LAZARUS_ROOT_DIR)
    set(LAZARUS_LCL_DIR "${LAZARUS_ROOT_DIR}/lcl")
    set(LAZARUS_COMPONENTS_DIR "${LAZARUS_ROOT_DIR}/components")
    set(LAZARUS_PACKAGER_DIR "${LAZARUS_ROOT_DIR}/packager")
    set(LAZARUS_DEBUGGER_DIR "${LAZARUS_ROOT_DIR}/debugger")
    set(LAZARUS_IDEINTF_DIR "${LAZARUS_ROOT_DIR}/ideintf")
    
    # Find compiled units directory
    if(EXISTS "${LAZARUS_ROOT_DIR}/lcl/units")
        set(LAZARUS_UNITS_DIR "${LAZARUS_ROOT_DIR}/lcl/units")
    endif()
    
    # Find FPC bundled with Lazarus
    if(WIN32)
        find_program(FPC_FROM_LAZARUS
            NAMES fpc.exe
            PATHS "${LAZARUS_ROOT_DIR}/fpc"
            PATH_SUFFIXES bin/i386-win32 bin/x86_64-win64 bin
            NO_DEFAULT_PATH
        )
    elseif(APPLE)
        find_program(FPC_FROM_LAZARUS
            NAMES fpc
            PATHS "${LAZARUS_ROOT_DIR}/fpc"
            PATH_SUFFIXES bin/i386-darwin bin/x86_64-darwin bin/aarch64-darwin bin
            NO_DEFAULT_PATH
        )
    else()
        find_program(FPC_FROM_LAZARUS
            NAMES fpc
            PATHS "${LAZARUS_ROOT_DIR}/fpc"
            PATH_SUFFIXES bin bin/i386-linux bin/x86_64-linux bin/aarch64-linux
            NO_DEFAULT_PATH
        )
    endif()
endif()

# Detect platform-specific information
if(LAZARUS_ROOT_DIR)
    if(APPLE)
        execute_process(
            COMMAND uname -m
            OUTPUT_VARIABLE LAZARUS_MACOS_ARCH
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
        )
        
        if(LAZARUS_MACOS_ARCH STREQUAL "arm64")
            set(LAZARUS_PLATFORM "macOS Apple Silicon (ARM64)")
        else()
            set(LAZARUS_PLATFORM "macOS Intel (x86_64)")
        endif()
    elseif(WIN32)
        if(CMAKE_SIZEOF_VOID_P EQUAL 8)
            set(LAZARUS_PLATFORM "Windows 64-bit")
        else()
            set(LAZARUS_PLATFORM "Windows 32-bit")
        endif()
    else()
        set(LAZARUS_PLATFORM "Linux")
    endif()
endif()

# Handle standard find_package arguments
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Lazarus
    REQUIRED_VARS LAZARUS_ROOT_DIR LAZBUILD_EXECUTABLE
    VERSION_VAR LAZARUS_VERSION
    FAIL_MESSAGE "Could not find Lazarus IDE. Please install Lazarus or set LAZARUS_ROOT_DIR manually."
)

mark_as_advanced(
    LAZARUS_EXECUTABLE
    LAZBUILD_EXECUTABLE
    LAZARUS_ROOT_DIR
    FPC_FROM_LAZARUS
)

# Print information if found
if(LAZARUS_FOUND)
    message(STATUS "Lazarus IDE found:")
    message(STATUS "  Root directory: ${LAZARUS_ROOT_DIR}")
    message(STATUS "  Version: ${LAZARUS_VERSION}")
    message(STATUS "  Platform: ${LAZARUS_PLATFORM}")
    message(STATUS "  IDE executable: ${LAZARUS_EXECUTABLE}")
    message(STATUS "  lazbuild: ${LAZBUILD_EXECUTABLE}")
    message(STATUS "  LCL directory: ${LAZARUS_LCL_DIR}")
    if(FPC_FROM_LAZARUS)
        message(STATUS "  Bundled FPC: ${FPC_FROM_LAZARUS}")
    endif()
endif()

# Function to build Lazarus projects
function(add_lazarus_project target)
    set(options)
    set(oneValueArgs PROJECT_FILE OUTPUT_NAME BUILD_MODE)
    set(multiValueArgs COMPILE_FLAGS)
    cmake_parse_arguments(LAZ "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
    
    if(NOT LAZ_PROJECT_FILE)
        message(FATAL_ERROR "add_lazarus_project: PROJECT_FILE argument is required")
    endif()
    
    if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${LAZ_PROJECT_FILE}")
        message(FATAL_ERROR "add_lazarus_project: Project file ${LAZ_PROJECT_FILE} does not exist")
    endif()
    
    # Set build mode (default to Default which doesn't require --build-mode flag)
    if(NOT LAZ_BUILD_MODE)
        set(LAZ_BUILD_MODE "Default")
    endif()
    
    # Determine output executable name
    get_filename_component(PROJECT_NAME ${LAZ_PROJECT_FILE} NAME_WE)
    if(NOT LAZ_OUTPUT_NAME)
        set(LAZ_OUTPUT_NAME ${PROJECT_NAME})
    endif()
    
    # Output path - each target must have unique output
    set(OUTPUT_PATH "${CMAKE_CURRENT_BINARY_DIR}/${LAZ_OUTPUT_NAME}${CMAKE_EXECUTABLE_SUFFIX}")
    
    # Get absolute path to project file
    get_filename_component(ABS_PROJECT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/${LAZ_PROJECT_FILE}" ABSOLUTE)
    get_filename_component(PROJECT_DIR "${ABS_PROJECT_FILE}" DIRECTORY)
    
    # Determine where lazbuild will actually place the output
    set(LAZARUS_OUTPUT_PATH "${PROJECT_DIR}/${PROJECT_NAME}${CMAKE_EXECUTABLE_SUFFIX}")
    
    # Build command - only add --build-mode if not Default
    set(BUILD_COMMAND ${LAZBUILD_EXECUTABLE})
    
    if(NOT LAZ_BUILD_MODE STREQUAL "Default")
        list(APPEND BUILD_COMMAND --build-mode=${LAZ_BUILD_MODE})
    endif()
    
    list(APPEND BUILD_COMMAND
        --build-all
        --quiet
        ${LAZ_COMPILE_FLAGS}
        ${ABS_PROJECT_FILE}
    )
    
    # Create custom command that builds and then copies to the correct location
    add_custom_command(
        OUTPUT ${OUTPUT_PATH}
        COMMAND ${BUILD_COMMAND}
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LAZARUS_OUTPUT_PATH} ${OUTPUT_PATH}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS ${ABS_PROJECT_FILE}
        COMMENT "Building Lazarus project ${LAZ_OUTPUT_NAME} (${LAZ_BUILD_MODE})"
        VERBATIM
    )
    
    # Create custom target that will build by default
    add_custom_target(${target} ALL
        DEPENDS ${OUTPUT_PATH}
    )
    
    # Set properties
    set_target_properties(${target} PROPERTIES
        LAZARUS_PROJECT_FILE ${LAZ_PROJECT_FILE}
        LAZARUS_BUILD_MODE ${LAZ_BUILD_MODE}
        LAZARUS_OUTPUT_PATH ${OUTPUT_PATH}
    )
endfunction()