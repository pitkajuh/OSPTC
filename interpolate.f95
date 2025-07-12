module interpolate
  implicit none
contains

  function linear_interpolation(array, energy, ny)
    integer :: nx, ny, i
    real :: energy, linear_interpolation
    real, dimension(:, :) :: array

    do i=1, ny
       if(array(1, i)>=energy) exit
    end do
    print *, energy, array(1, i-1), array(1, i), array(2, i-1), array(2, i)
    if(array(1, i)==energy) then
       linear_interpolation=array(2, i)
    else
       linear_interpolation=(array(2, i-1)+(energy-array(1, i-1))*(array(2, i)-array(2, i-1))/(array(1, i)-array(1, i-1)))
    end if
  end function linear_interpolation

end module interpolate
