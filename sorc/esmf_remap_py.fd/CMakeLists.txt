cmake_minimum_required(VERSION 3.16)
project(esmf_remap)
enable_language(Fortran C)

set(DDEFS "SPACK_PATH")
foreach(item ${DDEFS})
    list(APPEND DEFINITIONS "-D${item}")
endforeach(item ${DDEFS})
add_definitions(${DEFINITIONS})

set(CMAKE_Fortran_MODULE_DIRECTORY ${SPACK_PATH}/include)
set(CMAKE_Fortran_LIB_DIRECTORY ${SPACK_PATH}/include)
set(INSTALL_PATH ${CMAKE_CURRENT_SOURCE_DIR})
set(CMAKE_Fortran_FLAGS_RELEASE "-g -fcheck=all -fbacktrace -Wall")

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/sorc)


