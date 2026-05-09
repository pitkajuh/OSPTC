module interpolate
  use search
  implicit none
contains
  function linear_interpolation(array, x, nx) result(r)
    integer :: nx, i
    real(kind(1.d0)), intent(in) :: x
    real(kind(1.d0)) :: r
    real(kind(1.d0)), intent(in), dimension(:, :) :: array

    if(array(1, 1)==x) then
       i=1
       r=array(2, i)
    else if(array(1, nx)==x) then
       i=nx
       r=array(2, i)
    ! else if(x<array(1, 1))
    !    r=array(2, i-1)+(x-array(1, i-1))*(array(2, i)-&
    !         array(2, i-1))/(array(1, i)-array(1, i-1))
    ! else if(x>array(1, nx))
    !    r=array(2, i-1)+(x-array(1, i-1))*(array(2, i)-&
    !         array(2, i-1))/(array(1, i)-array(1, i-1))
    else
       i=binary_search(array, x, nx)

       r=array(2, i-1)+(x-array(1, i-1))*(array(2, i)-&
            array(2, i-1))/(array(1, i)-array(1, i-1))
       ! print *, "crh", i, r
    end if
  end function linear_interpolation
end module interpolate
