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

!! # TODO: Exceptions should be raised where indicated below;

!! # TODO: The missing value for the output variable arrays is assumed
!! # in instances when an element is >= 1.e20; this needs to be more
!! # general.

module regrid_io

  use module_ncio, only: Dataset, Dimension, Variable, &
       close_dataset, create_dataset, get_dim, get_var, open_dataset, &
       read_attribute, read_vardata, write_vardata
  use variables, only: esmf_struct, ilong, init_struct, maxchar, &
       nml_struct, rdouble, spval, varinfo_struct

  implicit none
  private

  public :: read_io, write_io

  interface read_io
     module procedure read_esmf
     module procedure read_varinfo
  end interface read_io

  interface write_io
     module procedure write_varinfo
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

    write(6,500) trim(adjustl(esmf%coeffs_file))
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

500 format("Reading the ESMF attributes from netCDF-formatted file path",1x,a,".")
  end subroutine read_esmf

  !----------------------------------------------------------------------------
  ! Subroutine: read_varinfo
  !
  ! Purpose: Reads the netCDF attributes and array values for the
  !          specified netCDF variable defined by the `varinfo_struct`
  !          variable `name` attribute.
  !
  ! Arguments:
  ! 
  !   varinfo[inout]: A FORTRAN `varinfo_struct` data structure.
  !----------------------------------------------------------------------------
  ! > @brief Reads the specified netCDF variable attributes.
  ! !
  ! ! @param[inout] varinfo
  ! !    - A FORTRAN `varinfo_struct` variable.
  ! !
  ! ! @return varinfo
  ! !    - A FORTRAN `varinfo_struct` variable populated with the
  ! !      respective netCDF variable attributes (e.g., information; -
  ! !      this includes the contents of the respective netCDF -
  ! !      variable array (see the `vararr` attribute)
  subroutine read_varinfo(varinfo)
    type(varinfo_struct), intent(inout) :: varinfo
    type(Dataset) :: varinfo_ds
    type(Variable) :: varinfo_attrs
    real(rdouble), dimension(:,:,:,:), allocatable :: vararr_4d
    real(rdouble), dimension(:,:,:), allocatable :: vararr_3d
    real(rdouble), dimension(:,:), allocatable :: vararr_2d
    integer(ilong) :: idx

    write(6,500) trim(adjustl(varinfo%name)), trim(adjustl(varinfo%file))
    varinfo_ds = open_dataset(varinfo%file)
    varinfo_attrs = get_var(varinfo_ds, trim(adjustl(varinfo%name)))
    varinfo%nx = varinfo_attrs%dimlens(1)
    varinfo%ny = varinfo_attrs%dimlens(2)

    varinfo%ndims = varinfo_attrs%ndims
    if (varinfo_attrs%ndims == 2) then
       varinfo%nz = 1
    else if (varinfo_attrs%ndims == 3) then
       varinfo%nz = varinfo_attrs%dimlens(3)
    else if (varinfo_attrs%ndims == 4) then
       varinfo%nz = varinfo_attrs%dimlens(3)
    else
       !! # TODO: Raise exception.
    end if
  
    call init_struct(varinfo)
    
    if (varinfo%ndims == 2) then
       if (.not. allocated(vararr_2d)) allocate(vararr_2d(varinfo%nx, varinfo%ny))
       call read_vardata(varinfo_ds, trim(adjustl(varinfo%name)), vararr_2d)
       varinfo%vararr(:,1) = reshape(vararr_2d, shape(varinfo%vararr(:,1)))
       if (allocated(vararr_2d)) deallocate(vararr_2d)
    else if (varinfo%ndims == 3) then
       if (.not. allocated(vararr_3d)) allocate(vararr_3d(varinfo%nx, varinfo%ny, varinfo%nz))
       call read_vardata(varinfo_ds, trim(adjustl(varinfo%name)), vararr_3d)
       do idx = 1, varinfo%nz
          varinfo%vararr(:,idx) = reshape(vararr_3d(:,:,idx), shape(varinfo%vararr(:,idx)))
       end do
       if (allocated(vararr_3d)) deallocate(vararr_3d)
    else if (varinfo%ndims == 4) then
       if (.not. allocated(vararr_4d)) allocate(vararr_4d(varinfo%nx, varinfo%ny, varinfo%nz, 1))
       call read_vardata(varinfo_ds, trim(adjustl(varinfo%name)), vararr_4d)
       do idx = 1, varinfo%nz
          varinfo%vararr(:,idx) = reshape(vararr_4d(:,:,idx, 1), shape(varinfo%vararr(:,idx)))
       end do
       if (allocated(vararr_4d)) deallocate(vararr_4d)
    end if
  
    call close_dataset(varinfo_ds)

500 format("Reading variable",1x,a,1x,"from netCDF-formatted file path",1x,a,".")
  end subroutine read_varinfo

  !----------------------------------------------------------------------------
  ! Subroutine: write_varinfo
  !
  ! Purpose: Writes the netCDF attributes and array values for the
  !          specified netCDF variable defined by the `varinfo_struct`
  !          variable `name` attribute.
  !
  ! Arguments:
  ! 
  !   varinfo[inout]: A FORTRAN `varinfo_struct` data structure.
  !----------------------------------------------------------------------------
  ! > @brief Writes the netCDF variable attributes.
  ! !
  ! ! @param[in] varinfo
  ! !    - A FORTRAN `varinfo_struct` variable.
  subroutine write_varinfo(varinfo)
    type(varinfo_struct), intent(in) :: varinfo
    type(Dataset) :: varinfo_ds_in, varinfo_ds_out
    real(rdouble), dimension(:,:,:,:), allocatable :: vararr_4d
    real(rdouble), dimension(:,:,:), allocatable :: vararr_3d
    real(rdouble), dimension(:,:), allocatable :: vararr_2d
    real(rdouble) :: missval
    integer(ilong) :: idx

    write(6,500) trim(adjustl(varinfo%name)), trim(adjustl(varinfo%file))
    varinfo_ds_in = open_dataset(trim(adjustl(varinfo%file)))   
    varinfo_ds_out = create_dataset(trim(adjustl(varinfo%ncoutput)), &
         varinfo_ds_in, copy_vardata=.true.)
    
    if (varinfo%ndims == 2) then
       if (.not. allocated(vararr_2d)) allocate(vararr_2d(varinfo%nx, varinfo%ny))
       vararr_2d = reshape(varinfo%vararr(:, 1), shape(vararr_2d))
       where(abs(vararr_2d) .ge. 1.e20) vararr_2d = 1.e30
       call write_vardata(varinfo_ds_out, trim(adjustl(varinfo%name)), vararr_2d)    
    else if (varinfo%ndims == 3) then
       if (.not. allocated(vararr_3d)) allocate(vararr_3d(varinfo%nx, varinfo%ny, varinfo%nz))
       do idx = 1, varinfo%nz
          vararr_3d(:,:,idx) = reshape(varinfo%vararr(:, idx), shape(vararr_3d(:,:,idx)))
       end do
       where(abs(vararr_3d) .ge. 1.e20) vararr_3d = 1.30
       call write_vardata(varinfo_ds_out, trim(adjustl(varinfo%name)), vararr_3d)
    else if (varinfo%ndims == 4) then
       if (.not. allocated(vararr_4d)) allocate(vararr_4d(varinfo%nx, varinfo%ny, varinfo%nz, 1))
       do idx = 1, varinfo%nz
          vararr_4d(:,:,idx,1) = reshape(varinfo%vararr(:, idx), shape(vararr_4d(:,:,idx,1)))
       end do
       where(abs(vararr_4d) .ge. 1.e20) vararr_4d = 1.30
       call write_vardata(varinfo_ds_out, trim(adjustl(varinfo%name)), vararr_4d)       
    else
       !! # TODO: Raise exception.
       stop
    end if

    if (allocated(vararr_2d)) deallocate(vararr_2d)
    if (allocated(vararr_3d)) deallocate(vararr_3d)
    if (allocated(vararr_4d)) deallocate(vararr_4d)
    call close_dataset(varinfo_ds_in)
    call close_dataset(varinfo_ds_out)
    
500 format("Writing variable",1x,a,1x,"to netCDF-formatted file path",1x,a,".")
  end subroutine write_varinfo
end module regrid_io
