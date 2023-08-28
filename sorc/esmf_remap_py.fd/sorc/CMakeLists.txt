
# Include directories
include_directories(${CMAKE_SOURCE_DIR}/sorc ${SPACK_PATH}/include)

# Set compiler flags
set(CMAKE_Fortran_FLAGS "-O3 -g -fcheck=all -fbacktrace -Wall")

# List of source files
file(GLOB f90_objs "*.F90")

# Add executable target
add_executable(esmf_remap ${f90_objs})

# Link libraries
set(LLIBS "m")
target_link_libraries(esmf_remap PRIVATE ${LLIBS})
target_include_directories(esmf_remap PRIVATE ${SPACK_PATH}/include)

# Install target
set(INSTALL_PATH "./")
install(TARGETS esmf_remap DESTINATION ${INSTALL_PATH})
