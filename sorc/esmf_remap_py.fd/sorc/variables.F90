! > @file sorc/variables.F90
! ! @details This module contains data structure applications and
! !          global variable declarations.
! ! @author Henry R. Winterbottom
! ! @date 31 August 2023
! ! @version 0.0.1
! ! @license LGPL v2.1
!------------------------------------------------------------------------------
! Module: variables
!
! Purpose: This module provides data structure and variable
!          declarations.
!          
! Authors: Henry R. Winterbottom
!
! Created: 31 August 2023
!
! Modified: 31 August 2023 -- Original version.
!------------------------------------------------------------------------------
module variables

  implicit none
  private

  public :: destroy_struct, esmf_struct, init_struct, ncvar_struct, nml_struct, &
       varinfo_struct

  integer, public, parameter :: ilong = selected_int_kind(8)
  integer, public, parameter :: maxchar = 1024
  integer, public, parameter :: rdouble = selected_real_kind(15)
  integer, public, parameter :: rsingle = selected_real_kind(6)
  real, public, parameter :: spval = tiny(1.0)
  
  interface destroy_struct
     module procedure destroy_esmf_struct
     module procedure destroy_varinfo_struct
  end interface destroy_struct

  interface init_struct
     module procedure init_esmf_struct
     module procedure init_varinfo_struct
  end interface init_struct
  
  type esmf_struct
     character(len=maxchar) :: coeffs_file
     real(rdouble), dimension(:), allocatable :: area_a
     real(rdouble), dimension(:), allocatable :: area_b
     real(rdouble), dimension(:), allocatable :: frac_a
     real(rdouble), dimension(:), allocatable :: frac_b
     real(rdouble), dimension(:), allocatable :: s
     integer(ilong), dimension(:), allocatable :: col
     integer(ilong), dimension(:), allocatable :: dst_grid_dims
     integer(ilong), dimension(:), allocatable :: mask_a
     integer(ilong), dimension(:), allocatable :: mask_b
     integer(ilong), dimension(:), allocatable :: row
     integer(ilong), dimension(:), allocatable :: src_grid_dims
     integer(ilong) :: dst_grid_rank
     integer(ilong) :: n_a
     integer(ilong) :: n_b
     integer(ilong) :: n_s
     integer(ilong) :: src_grid_rank
  end type esmf_struct

  type ncvar_struct
     character(len=maxchar) :: file
     character(len=maxchar) :: varname
  end type ncvar_struct
  
  type nml_struct
     character(len=maxchar) :: esmf_coeffs_file
     character(len=maxchar) :: invar_file
     character(len=maxchar) :: invar_name
     character(len=maxchar) :: ncoutput
     character(len=maxchar) :: outvar_file
     character(len=maxchar) :: outvar_name
  end type nml_struct

  type varinfo_struct
     character(len=maxchar) :: file
     character(len=maxchar) :: name
     character(len=maxchar) :: ncoutput
     real(rdouble), dimension(:,:), allocatable :: vararr
     integer(ilong) :: nx
     integer(ilong) :: ny
     integer(ilong) :: nz
  end type varinfo_struct

contains

  !----------------------------------------------------------------------------
  ! Subroutine: destroy_esmf_struct
  !
  ! Purpose: Deallocate all variable arrays within the `esmf_struct`
  !          data structure.
  !
  ! Arguments:
  ! 
  !   struct[in]: A FORTRAN `esmf_struct` data structure.
  ! ----------------------------------------------------------------------------
  ! > @brief Deallocates the allocated variable arrays within the data
  ! !        structure.
  ! !
  ! ! @param[in] struct
  ! !    - A FORTRAN `esmf_struct` variable.
  subroutine destroy_esmf_struct(struct)
    type(esmf_struct) :: struct

    if (allocated(struct%area_a)) deallocate(struct%area_a)
    if (allocated(struct%area_b)) deallocate(struct%area_b)
    if (allocated(struct%frac_a)) deallocate(struct%frac_a)
    if (allocated(struct%frac_b)) deallocate(struct%frac_b)
    if (allocated(struct%s)) deallocate(struct%s)
    if (allocated(struct%col)) deallocate(struct%col)
    if (allocated(struct%dst_grid_dims)) deallocate(struct%dst_grid_dims)
    if (allocated(struct%mask_a)) deallocate(struct%mask_a)
    if (allocated(struct%mask_b)) deallocate(struct%mask_b)
    if (allocated(struct%row)) deallocate(struct%row)
    if (allocated(struct%src_grid_dims)) deallocate(struct%src_grid_dims)
  end subroutine destroy_esmf_struct

  !----------------------------------------------------------------------------
  ! Subroutine: destroy_varinfo_struct
  !
  ! Purpose: Deallocate all variable arrays within the `varinfo_struct`
  !          data structure.
  !
  ! Arguments:
  ! 
  !   struct[in]: A FORTRAN `varinfo_struct` data structure.
  ! ----------------------------------------------------------------------------
  ! > @brief Deallocates the allocated variable arrays within the data
  ! !        structure.
  ! !
  ! ! @param[in] struct
  ! !    - A FORTRAN `varinfo_struct` variable.
  subroutine destroy_varinfo_struct(struct)
    type(varinfo_struct) :: struct
    
    if (allocated(struct%vararr)) deallocate(struct%vararr)
  end subroutine destroy_varinfo_struct

  !----------------------------------------------------------------------------
  ! Subroutine: init_esmf_struct
  !
  ! Purpose: Allocate all variable arrays within the `esmf_struct`
  !          data structure.
  !
  ! Arguments:
  ! 
  !   struct[inout]: A FORTRAN `esmf_struct` data structure.
  ! ----------------------------------------------------------------------------
  ! > @brief Allocates variable arrays within the data structure.
  ! !
  ! ! @param[inout] struct
  ! !    - A FORTRAN `esmf_struct` variable.
  subroutine init_esmf_struct(struct)
    type(esmf_struct) :: struct

    if (.not. allocated(struct%area_a)) allocate(struct%area_a(struct%n_a))
    if (.not. allocated(struct%area_b)) allocate(struct%area_b(struct%n_b))
    if (.not. allocated(struct%frac_a)) allocate(struct%frac_a(struct%n_a))
    if (.not. allocated(struct%frac_b)) allocate(struct%frac_b(struct%n_b))
    if (.not. allocated(struct%s)) allocate(struct%s(struct%n_s))
    if (.not. allocated(struct%col)) allocate(struct%col(struct%n_s))
    if (.not. allocated(struct%dst_grid_dims)) &
         allocate(struct%dst_grid_dims(struct%dst_grid_rank))
    if (.not. allocated(struct%mask_a)) allocate(struct%mask_a(struct%n_a))
    if (.not. allocated(struct%mask_b)) allocate(struct%mask_b(struct%n_b))
    if (.not. allocated(struct%row)) allocate(struct%row(struct%n_s))
    if (.not. allocated(struct%src_grid_dims)) &
         allocate(struct%src_grid_dims(struct%src_grid_rank))
  end subroutine init_esmf_struct

  !----------------------------------------------------------------------------
  ! Subroutine: init_varinfo_struct
  !
  ! Purpose: Allocate all variable arrays within the `varinfo_struct`
  !          data structure.
  !
  ! Arguments:
  ! 
  !   struct[inout]: A FORTRAN `esmf_struct` data structure.
  ! ----------------------------------------------------------------------------
  ! > @brief Allocates variable arrays within the data structure.
  ! !
  ! ! @param[inout] struct
  ! !    - A FORTRAN `varinfo_struct` variable.
  subroutine init_varinfo_struct(struct)
    type(varinfo_struct) :: struct

    if (.not. allocated(struct%vararr)) &
         allocate(struct%vararr(struct%nx*struct%ny, struct%nz))
  end subroutine init_varinfo_struct
  
end module variables
