! > @file sorc/regrid.F90
! ! @details This is the top-level interface to the regridding application.
! ! @author Henry R. Winterbottom
! ! @date 31 August 2023
! ! @version 0.0.1
! ! @license LGPL v2.1
!------------------------------------------------------------------------------
! Program: regrid
!
! Purpose: Interface to the regridding application.
!          
! Authors: Henry R. Winterbottom
!
! Created: 31 August 2023
!
! Modified: 31 August 2023 -- Original version.
!------------------------------------------------------------------------------
program regrid

  use interp, only: esmf_interp
  use namelist, only: read_namelist
  use regrid_io, only: read_io, write_io
  use variables, only: destroy_struct, esmf_struct, ilong, maxchar, nml_struct, varinfo_struct

  implicit none
  type(esmf_struct) :: esmf
  type(nml_struct) :: nml
  type(varinfo_struct) :: varinfo_in
  type(varinfo_struct) :: varinfo_out
  character(len=maxchar) :: ftnnml
  integer(ilong) :: idx

  call get_command_argument(1, ftnnml)
  nml = read_namelist(nml_file=ftnnml)
  varinfo_in%file = nml%invar_file
  varinfo_in%name = nml%invar_name
  varinfo_out%file = nml%outvar_file
  varinfo_out%name = nml%outvar_name  
  varinfo_out%ncoutput = nml%ncoutput
  esmf%coeffs_file = nml%esmf_coeffs_file
  
  call read_io(esmf)
  call read_io(varinfo_in)
  call read_io(varinfo_out)

  call esmf_interp(esmf=esmf, varin=varinfo_in%vararr, &
       varout=varinfo_out%vararr)

  call write_io(varinfo_out)
  call destroy_struct(esmf)
  call destroy_struct(varinfo_in)
  call destroy_struct(varinfo_out)
  
end program regrid
