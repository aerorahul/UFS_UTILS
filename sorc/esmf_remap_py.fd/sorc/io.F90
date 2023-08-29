! > @file sorc/io.F90
! ! @details This module contains input/output (I/O) interfaces.
! ! @author Henry R. Winterbottom
! ! @date 28 August 2023
! ! @version 0.0.1
! ! @license LGPL v2.1
!------------------------------------------------------------------------------
! Module: regrid_io
!
! Purpose: This module provides I/O operations for reading and writing
!          data netCDF-formatted data.
!          
! Authors: Henry R. Winterbottom
!
! Created: 28 August 2023
!
! Modified: 28 August 2023 -- Original version.
!------------------------------------------------------------------------------
module regrid_io

  use module_ncio, only: Dataset, Dimension, close_dataset, get_dim, open_dataset, read_vardata
  use variables, only: esmf_struct, ilong, init_struct, maxchar, nml_struct

  implicit none
  private

  public :: read_io, write_io

  interface read_io
     module procedure read_esmf
  end interface read_io

  interface write_io
  end interface write_io

contains

  !----------------------------------------------------------------------------
  ! Subroutine: read_esmf
  !
  ! Purpose: Reads ESMF regridding-related data from a dataset and
  !          populates the `esmf_struct` with the retrieved information.
  !
  ! Arguments:
  ! 
  !   esmf[inout]: A FORTRAN `esmf_struct` data structure.
  !----------------------------------------------------------------------------
  ! > @brief Reads the ESMF remapping attributes.
  ! !
  ! ! @param[inout] esmf
  ! !    - A FORTRAN `esmf_struct` variable.
  ! !
  ! ! @return esmf
  ! !    - A FORTRAN `esmf_struct` variable populated with the ESMF
  ! !      remapping attributes.
  subroutine read_esmf(esmf)
    type(esmf_struct), intent(inout) :: esmf
    type(Dataset) :: esmf_ds
    type(Dimension) :: esmf_dim

    esmf_ds = open_dataset(esmf%coeffs_file)
    esmf_dim = get_dim(dset=esmf_ds, dimname="dst_grid_rank")
    esmf%dst_grid_rank = esmf_dim%len
    esmf_dim = get_dim(dset=esmf_ds, dimname="n_a")
    esmf%n_a = esmf_dim%len
    esmf_dim = get_dim(dset=esmf_ds, dimname="n_b")
    esmf%n_b = esmf_dim%len
    esmf_dim = get_dim(dset=esmf_ds, dimname="n_s")
    esmf%n_s = esmf_dim%len
    esmf_dim = get_dim(dset=esmf_ds, dimname="src_grid_rank")
    esmf%src_grid_rank = esmf_dim%len
    
    call init_struct(esmf)
    
    call read_vardata(esmf_ds, "area_a", esmf%area_a)
    call read_vardata(esmf_ds, "area_b", esmf%area_b)
    call read_vardata(esmf_ds, "col", esmf%col)
    call read_vardata(esmf_ds, "frac_a", esmf%frac_a)
    call read_vardata(esmf_ds, "frac_b", esmf%frac_b)
    call read_vardata(esmf_ds, "mask_a", esmf%mask_a)
    call read_vardata(esmf_ds, "mask_b", esmf%mask_b)
    call read_vardata(esmf_ds, "row", esmf%row)
    call read_vardata(esmf_ds, "S", esmf%s) 
    
    call close_dataset(esmf_ds)
  end subroutine read_esmf

end module regrid_io
