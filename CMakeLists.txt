cmake_minimum_required(VERSION 3.10)

project(OSPTC)
enable_language(Fortran)

set(CMAKE_Fortran_FLAGS "-fopenmp -std=f2008")
set(CMAKE_Fortran_FLAGS_DEBUG "-Wall -Wline-truncation  -Wcharacter-truncation  -Wsurprising  -Waliasing  -Wimplicit-interface  -Wunused-parameter  -fwhole-file -fcheck=all -pedantic  -fbacktrace -fall-intrinsics")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native -flto")

file(GLOB_RECURSE sources *.f95)
add_executable(osptc ${sources})
set_target_properties(osptc PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}")
