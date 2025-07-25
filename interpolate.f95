module interpolate
  implicit none
contains

  function linear_interpolation(array, x, nx) result(r)
    integer :: nx, i
    real(kind(1.d0)), intent(in) :: x
    real(kind(1.d0)) :: r
    real(kind(1.d0)), intent(in), dimension(:, :) :: array
    ! A naive solution. Binary search would be better.
    r=0.0_8

    do i=1, nx
       if(array(1, i)>=x) exit
    end do
    ! if(i>=nx) then
    !    i=i-1
    !    r=array(2, i-1)+(array(2, i)-array(2, i-1))*((x-array(1, i-1))/(array(1, i)-array(1, i-1)))
    ! else
       r=array(2, i-1)+(x-array(1, i-1))*(array(2, i)-array(2, i-1))/(array(1, i)-array(1, i-1))
    ! endif
  end function linear_interpolation
end module interpolate
