! > @file sorc/namelist.F90
! ! @details This module contains the FORTRAN namelist interface.
! ! @author Henry R. Winterbottom
! ! @date 28 August 2023
! ! @version 0.0.1
! ! @license LGPL v2.1
!------------------------------------------------------------------------------
! Module: namelist
!
! Purpose: This module provides an interface to the FORTRAN
!          namelist-formatted file.
!          
! Authors: Henry R. Winterbottom
!
! Created: 28 August 2023
!
! Modified: 28 August 2023 -- Original version.
!------------------------------------------------------------------------------
module namelist
  use variables, only: ilong, maxchar, nml_struct

  implicit none
  private

  public :: read_namelist

  character(len=maxchar) :: esmf_coeffs_file
  character(len=maxchar) :: invar_file
  character(len=maxchar) :: invar_name
  character(len=maxchar) :: outvar_file
  character(len=maxchar) :: outvar_name
  
  integer(ilong), parameter :: unit_nml = 99
  
  namelist / esmf / esmf_coeffs_file, &
       invar_file, &
       invar_name, &
       outvar_file, &
       outvar_name

contains

  !----------------------------------------------------------------------------
  ! Subroutine: read_namelist
  !
  ! Purpose: Reads namelist file and populates the `nml_struct` with
  !          the retrieved information.
  !
  ! Arguments:
  ! 
  !   nml[inout]: A FORTRAN `nml_struct` data structure.
  !----------------------------------------------------------------------------
  
  ! > @brief Reads the FORTRAN namelist-formatted file and returns a
  ! ! `nml_struct` variable containing the contents within.
  ! !
  ! ! @params[in] nml_file
  ! !    - The FORTRAN namelist-formatted file path.
  ! !
  ! ! @returns nml
  ! !    - A FORTRAN `nml_struct` variable containing the contents of
  ! !      the FORTRAN namelist-formatted file path.
  function read_namelist(nml_file) result(nml)
    type(nml_struct) :: nml
    character(len=maxchar), intent(in) :: nml_file
    
    open(file=trim(adjustl(nml_file)), unit=unit_nml, status="old", &
         form="formatted", action="read")
    read(unit_nml, NML = esmf)
    close(unit_nml)
    
    nml%esmf_coeffs_file = esmf_coeffs_file
    nml%invar_file = invar_file
    nml%invar_name = invar_name
    nml%outvar_file = outvar_file
    nml%outvar_name = outvar_name
  end function read_namelist

end module namelist
