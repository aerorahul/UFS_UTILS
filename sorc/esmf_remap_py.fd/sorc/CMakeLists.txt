


file(GLOB f90_objs "*.F90")
set(TARGET "esmf_remap")
set_target_properties(esmf_remap.x PROPERTIES Fortan_MODULE_DIRECTORY "${SPACK_PATH}/include")
target_link_libraries(esmf_remap PRIVATE "${SPACK_PATH}/lib")
#include_directories(${CMAKE_SOURCE_DIR}/sorc ${SPACK_PATH}/include)
set(CMAKE_Fortran_FLAGS "-O3 -g -fcheck=all -fbacktrace -Wall")


#set(LLIBS "m" ${SPACK_PATH}/lib -lnetcdf -lnetcdff -lncio)
#set(SPACK_PATH "



#target_include_directories(esmf_remap PRIVATE ${SPACK_PATH}/include)
#target_link_libraries(esmf_remap PRIVATE ${LLIBS})
add_executable(esmf_remap ${f90_objs})


set(INSTALL_PATH "./")
install(TARGETS esmf_remap DESTINATION ${INSTALL_PATH})
