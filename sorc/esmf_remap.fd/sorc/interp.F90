! > @file sorc/interp.F90
! ! @details This module contains interpolation interfaces.
! ! @author Henry R. Winterbottom
! ! @date 31 August 2023
! ! @version 0.0.1
! ! @license LGPL v2.1
!------------------------------------------------------------------------------
! Module: interp
!
! Purpose: This module provides interpolation algorithm interfaces.
!          
! Authors: Henry R. Winterbottom
!
! Created: 31 August 2023
!
! Modified: 31 August 2023 -- Original version.
!------------------------------------------------------------------------------
module interp
  use variables, only: esmf_struct, ilong, maxchar, rdouble

  implicit none
  private
  
  public :: esmf_interp

contains

  !----------------------------------------------------------------------------
  ! Subroutine: esmf_interp
  !
  ! Purpose: Interpolates a variable defined on a source grid
  !          projection to a destination grid projection using the
  !          ESMF remapping attributes.
  !
  ! Arguments:
  ! 
  !   esmf[in]: A FORTRAN `esmf_struct` data structure.
  !
  !   varin[in]: A FORTRAN `rdouble` array.
  !----------------------------------------------------------------------------
  ! > @brief Reads the ESMF remapping attributes.
  ! !
  ! ! @param[in] esmf
  ! !    - A FORTRAN `esmf_struct` variable populated with the ESMF
  ! !      remapping attributes.
  ! !
  ! ! @param[in] varin
  ! !    - A FORTRAN `rdouble` array containing the input variable array
  ! !      define on the source grid projection.
  ! !
  ! ! @return varout
  ! !    - A FORTRAN `rdouble` array containing the input variable array,
  ! !      `varin`, interpolated to the destination grid projection.
  subroutine esmf_interp(esmf, varin, varout)
    type(esmf_struct), intent(in) :: esmf
    real(rdouble), intent(in) :: varin(:,:)
    real(rdouble), intent(out) :: varout(:,:) 
    integer(ilong) :: idx, jdx

    do jdx = 1, size(varin(1, :))
       varout(:, jdx) = 0.0_rdouble
       do idx = 1, esmf%n_s
          varout(esmf%row(idx), jdx) = &
               varout(esmf%row(idx), jdx) + &
               esmf%s(idx) * varin(esmf%col(idx), jdx)
       end do
    end do
  end subroutine esmf_interp

end module interp
