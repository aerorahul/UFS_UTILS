add_subdirectory(sorc)

target_link_libraries(
  NetCDF::NetCDF_Fortran
  ncio)
