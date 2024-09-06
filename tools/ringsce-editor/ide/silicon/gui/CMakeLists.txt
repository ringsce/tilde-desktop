cmake_minimum_required(VERSION 3.19.2)
project(window C)
set(CMAKE_APPLE_SILICON_PROCESS, arm64)
set(CMAKE_C_STANDARD 11)

include_directories(.)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c11")

set(SOURCE_FILES mainwindow.m)
add_executable(gtk_test ${SOURCE_FILES})

find_package(PkgConfig REQUIRED)
pkg_check_modules(GTK3 REQUIRED gtk+-3.0)

include_directories(${GTK3_INCLUDE_DIRS})
link_directories(${GTK3_LIBRARY_DIRS})

add_definitions(${GTK3_CFLAGS_OTHER})

target_link_libraries(gtk_test ${GTK3_LIBRARIES})

add_executable(window
        mainwindow.m)