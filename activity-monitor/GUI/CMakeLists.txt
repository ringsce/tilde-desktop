cmake_minimum_required(VERSION 3.16)
project(ActivityMonitor)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_AUTOMOC ON)

find_package(Qt6 REQUIRED COMPONENTS Core Gui Widgets)

add_executable(ActivityMonitor main.cpp)
target_link_libraries(ActivityMonitor Qt6::Core Qt6::Gui Qt6::Widgets)
