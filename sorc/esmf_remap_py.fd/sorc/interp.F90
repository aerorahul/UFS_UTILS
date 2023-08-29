module interp
  use variables, only: esmf_struct, ilong, maxchar, rdouble

  implicit none
  private
  public :: esmf_interp
contains

  function esmf_interp(esmf, varin) result(varout)
    type(esmf_struct), intent(in) :: esmf
    real(rdouble), intent(in), dimension(:) :: varin
    real(rdouble), dimension(:), allocatable :: varout
    integer(ilong) :: idx

    varout = 0.0_rdouble
    do idx = 1, esmf%n_s
       varout(esmf%row(idx)) = varout(esmf%row(idx)) &
            + esmf%s(idx)*varin(esmf%col(idx))
    end do
  end function esmf_interp

end module interp
