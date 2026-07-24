module interpolate
  use search
  implicit none
contains
  function linear_interpolation(array, x, nx, axis1, axis2) result(r)
    integer, intent(in) :: nx, axis1, axis2
    real(kind(1.d0)), intent(in) :: x
    real(kind(1.d0)), intent(in), dimension(:, :) :: array
    integer :: i
    real(kind(1.d0)) :: r
    r=0.0_8
    i=1

    if(array(axis1, 1)==x) then
       i=1
       r=array(axis2, i)
    else if(array(axis1, nx)==x) then
       i=nx
       r=array(axis2, i)
    else if(x<array(1, 1)) then
       r=x*array(2, i)/array(1, i)
    else if(x>array(1, nx)) then
       ! Needs extrapolation
       print *, "Houston2, we have a problem, want extrapolation", x
       print *, array(axis1, 1), array(axis2, 1)
       print *, array(axis1, 2), array(axis2, 2)
       print *, array(axis1, 3), array(axis2, 3)
       error stop
    else if(array(axis1, 1)==array(axis1, 2) .and. x<=array(axis1, 3)) then
       i=2
       r=array(axis2, i-1)+(x-array(axis1, i-1))*(array(axis2, i)-&
            array(axis2, i-1))/(array(axis1, i)-array(axis1, i-1))
    else
       i=binary_search(array, x, nx, axis1)

       ! if(array(axis1, 1)==array(axis1, 2) .and. x<=array(axis1, 3)) then
       !    ! print *, "help?", x
       !    ! print *, array(axis1, 1), array(axis2, 1)
       !    ! print *, array(axis1, 2), array(axis2, 2)
       !    ! print *, array(axis1, 3), array(axis2, 3)
       !    i=2
       ! end if
       ! print *, "i=", i
       r=array(axis2, i-1)+(x-array(axis1, i-1))*(array(axis2, i)-&
            array(axis2, i-1))/(array(axis1, i)-array(axis1, i-1))
    end if

  end function linear_interpolation
end module interpolate
