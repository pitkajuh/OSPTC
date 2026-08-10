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
    ! logical :: ch
    ! ch=x>=array(axis1, 2) .and. array(axis1, 3)>=x
    r=0.0_8
    i=1
    ! print *, array(axis1, 1)==array(axis1, 2), x>=array(axis1, 2), x<=array(axis1, 3)
    ! print *, array(axis1, 1), array(axis1, 2), array(axis1, 3), x
    if(array(axis1, 1)==x) then
       i=1
       r=array(axis2, i)
    else if(array(axis1, nx)==x) then
       i=nx
       r=array(axis2, i)
    ! else if(x<array(axis1, 1)) then
    !    r=x*array(axis2, i)/array(axis1, i)
    else if(x>array(axis1, nx)) then
       ! Needs extrapolation
       print *, "Houston, we have a problem, want extrapolation", x
       print *, array(axis1, 1), array(axis2, 1)
       print *, array(axis1, 2), array(axis2, 2)
       print *, array(axis1, 3), array(axis2, 3)
       error stop
    else if(array(axis1, 1)==array(axis1, 2) .and. (x<=array(axis1, 3) .and. x>=array(axis1, 2))) then
    ! else if(array(axis1, 1)==0.0_8 .and. array(axis1, 2)==0.0_8 .and. (x<=array(axis1, 3) .and. x>=array(axis1, 2))) then
       ! Photoionization has two same values.
       ! print *, "ION"
       i=3
       r=array(axis2, i-1)+(x-array(axis1, i-1))*(array(axis2, i)-&
            array(axis2, i-1))/(array(axis1, i)-array(axis1, i-1))
    ! else if(array(axis1, 1)) then

    ! else if(array(axis1, 1)==array(axis1, 2) .and. x<=array(axis1, 3)) then
    !    i=2
    !    r=array(axis2, i-1)+(x-array(axis1, i-1))*(array(axis2, i)-&
       !         array(axis2, i-1))/(array(axis1, i)-array(axis1, i-1))
    else if(array(axis2, 1)==0.0_8 .and. array(axis1, 1)<x .and. array(axis1, 2)>x) then
       i=2
       ! print *, "ION?"
       r=array(axis2, i-1)+(x-array(axis1, i-1))*(array(axis2, i)-&
            array(axis2, i-1))/(array(axis1, i)-array(axis1, i-1))
    else
       i=binary_search(array, x, nx, axis1)
       ! print *, "ii=", i, x, array(axis1, 1), array(axis1, 2), array(axis1, 3), x
       ! if(array(axis1, 1)==array(axis1, 2) .and. x<=array(axis1, 3)) then
       !    ! print *, "help?", x
       !    ! print *, array(axis1, 1), array(axis2, 1)
       !    ! print *, array(axis1, 2), array(axis2, 2)
       !    ! print *, array(axis1, 3), array(axis2, 3)
       !    i=2
       ! end if
       ! print *, "i=", i
       ! print *, array(axis2, i-1), (x-array(axis1, i-1)), (array(axis2, i)-array(axis2, i-1)), (array(axis1, i)-array(axis1, i-1))

       r=array(axis2, i-1)+(x-array(axis1, i-1))*(array(axis2, i)-&
            array(axis2, i-1))/(array(axis1, i)-array(axis1, i-1))
    end if

  end function linear_interpolation
end module interpolate
