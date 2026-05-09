module interpolate
  use search
  implicit none
contains
  function linear_interpolation(array, x, nx, axis1, axis2) result(r)
    integer :: i
    integer, intent(in) :: nx, axis1, axis2
    real(kind(1.d0)), intent(in) :: x
    real(kind(1.d0)) :: r
    real(kind(1.d0)), intent(in), dimension(:, :) :: array

    if(array(axis1, 1)==x) then
       i=1
       r=array(axis2, i)
    else if(array(axis1, nx)==x) then
       i=nx
       r=array(axis2, i)
    ! else if(x<array(1, 1))
    !    r=array(2, i-1)+(x-array(1, i-1))*(array(2, i)-&
    !         array(2, i-1))/(array(1, i)-array(1, i-1))
    ! else if(x>array(1, nx))
    !    r=array(2, i-1)+(x-array(1, i-1))*(array(2, i)-&
    !         array(2, i-1))/(array(1, i)-array(1, i-1))
    else
       i=binary_search(array, x, nx, axis1)
       r=array(axis2, i-1)+(x-array(axis1, i-1))*(array(axis2, i)-&
            array(axis2, i-1))/(array(axis1, i)-array(axis1, i-1))
    end if
  end function linear_interpolation
end module interpolate
