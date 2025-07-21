module interpolate
  implicit none
contains

  function linear_interpolation(array, y, ny)
    integer :: ny, i
    real(kind(1.d0)) :: y, linear_interpolation
    real(kind(1.d0)), dimension(:, :) :: array
    ! A naive solution. Binary search would be better.
    do i=1, ny
       if(array(1, i)>=y) exit
    end do
    if(i>=ny) print *, "problem", y
    linear_interpolation=array(2, i-1)+(y-array(1, i-1))*(array(2, i)-array(2, i-1))/(array(1, i)-array(1, i-1))
  end function linear_interpolation

  function linear_interpolation2(array, y, nx, ny)
    integer :: i, j
    integer, intent(in) :: nx, ny
    real(kind(1.d0)) :: y, linear_interpolation2
    real(kind(1.d0)), dimension(:, :) :: array
    logical :: found
    found=.false.
    i=1
    j=1
    ! A naive solution. Binary search would be better.
    do j=1, nx
       ! if(found.eqv..true.) then
       !    print *, j, "exit"
       !    exit
       ! end if
       do i=1, ny
          if(array(j, i)>=y) then
             print *, j, i, array(j, i), y, "found", array(j, i-1)
             ! found=.true.
             exit
          end if
       end do
       if(found.eqv..true.) exit
    end do

    linear_interpolation2=array(2, i-1)+(y-array(1, i-1))*(array(2, i)-array(2, i-1))/(array(1, i)-array(1, i-1))
  end function linear_interpolation2

end module interpolate
