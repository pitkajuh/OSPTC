cmake_minimum_required(VERSION 3.15)

project(OSPTC)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR})
 # set (CMAKE_CXX_FLAGS "-g -Wall -Werror -Wpedantic -fsanitize=address -p" )
  set (CMAKE_CXX_FLAGS "-g -Wall -Werror -Wpedantic -fsanitize=address" )
# set (CMAKE_CXX_FLAGS "-g -Wall -Werror -Wpedantic")
file(GLOB_RECURSE SRC_FILES *.cpp)
add_executable(main main.cpp ${SRC_FILES})
