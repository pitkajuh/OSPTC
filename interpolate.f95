module interpolate
  implicit none
contains

  function linear_interpolation(array, y, ny)
    integer :: ny, i
    real :: y, linear_interpolation
    real, dimension(:, :) :: array
    ! A naive solution. Binary search would be better.
    do i=1, ny
       if(array(1, i)>=y) exit
    end do

    linear_interpolation=array(2, i-1)+(y-array(1, i-1))*(array(2, i)-array(2, i-1))/(array(1, i)-array(1, i-1))
  end function linear_interpolation

end module interpolate
