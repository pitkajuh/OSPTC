module interpolate
  implicit none
contains

  function linear_interpolation(array, y, ny)
    integer :: ny, i
    real :: y, linear_interpolation
    real, dimension(:, :) :: array

    do i=1, ny
       if(array(1, i)>=y) exit
    end do

    if(array(1, i)==y) then
       linear_interpolation=array(2, i)
    else
       linear_interpolation=(array(2, i-1)+(y-array(1, i-1))*(array(2, i)-array(2, i-1))/(array(1, i)-array(1, i-1)))
    end if
  end function linear_interpolation

end module interpolate
