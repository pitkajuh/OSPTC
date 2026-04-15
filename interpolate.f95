module interpolate
  use search
  implicit none
contains
  function linear_interpolation(array, x, nx) result(r)
    integer :: nx, i
    real(kind(1.d0)), intent(in) :: x
    real(kind(1.d0)) :: r
    real(kind(1.d0)), intent(in), dimension(:, :) :: array
    i=binary_search(array, x, nx)
    ! print *, "find", x, "at ", i, nx
    r=array(2, i-1)+(x-array(1, i-1))*(array(2, i)-array(2, i-1))/(array(1, i)-array(1, i-1))
  end function linear_interpolation
end module interpolate
