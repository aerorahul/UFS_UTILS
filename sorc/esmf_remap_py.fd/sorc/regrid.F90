program regrid

  use interp, only: esmf_interp
  use namelist, only: read_namelist
  use regrid_io, only: read_io !!, write_io
  use variables, only: destroy_struct, esmf_struct, ilong, maxchar, nml_struct

  implicit none
  type(esmf_struct) :: esmf
  type(nml_struct) :: nml
  character(len=maxchar) :: ftnnml
  
  !! 
  call get_command_argument(1, ftnnml)
  nml = read_namelist(nml_file=ftnnml)
  
  esmf%coeffs_file = nml%esmf_coeffs_file
  call read_io(esmf)


  call destroy_struct(esmf)

  !! ESMF regridding coefficient file;
  !! Input netCDF file containing variable to be remapped;
  !! Destination (i.e., output) netCDF file varriable name;
  !! netCDF destination file.


  
  !! TODO: Read in the netCDF variable.

  !! TODO: Interpolate the netCDF variable.

  !! TODO: Write the netCDF variable.
  
end program regrid
