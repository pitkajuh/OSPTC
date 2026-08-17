module search
  implicit none
contains
    function binary_search_1d(array, x, nx) result(i)
    real(kind(1.d0)), intent(in), dimension(:) :: array
    real(kind(1.d0)), intent(in) :: x
    integer, intent(in) :: nx
    integer :: i, left, right
    real(kind(1.d0)) :: A1
    A1=0.0_8
    i=1
    left=1
    right=nx

    do
       if(left>=right) then
          exit
       end if

       A1=0.5*(right-left)
       i=left+floor(A1)

       if(i==1) then
          print *, "Houston, we have a problem in 1d.", x, nx, A1, left, right
          print *, array(1)
          print *, array(2)
          print *, array(3)
          error stop
       end if

       if(array(i)<x) then
          left=i+1
       else if(array(i)>x) then
          right=i-1
       else
          exit
       end if
    end do

  end function binary_search_1d

  function binary_search(array, x, nx, axis) result(i)
    real(kind(1.d0)), intent(in), dimension(:, :) :: array
    real(kind(1.d0)), intent(in) :: x
    integer, intent(in) :: nx, axis
    integer :: i, left, right
    real(kind(1.d0)) :: A1
    A1=0.0_8
    i=1
    left=1
    right=nx

    do
       if(left>=right) then
          exit
       end if

       A1=0.5*(right-left)
       i=left+floor(A1)

       if(i==1) then
          print *, "Houston, we have a problem in.", x, nx, A1, left, right
          print *, array(1, 1), array(2, 1)
          print *, array(1, 2), array(2, 2)
          print *, array(1, 3), array(2, 3)
          error stop
       end if

       if(array(axis, i)<x) then
          left=i+1
       else if(array(axis, i)>x) then
          right=i-1
       else
          exit
       end if
    end do
  end function binary_search
end module search
