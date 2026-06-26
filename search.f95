module search
  implicit none
contains
  function binary_search(array, x, nx, axis) result(i)
    real(kind(1.d0)), intent(in), dimension(:, :) :: array
    real(kind(1.d0)), intent(in) :: x
    integer, intent(in) :: nx, axis
    integer :: i, left, right
    real(kind(1.d0)) :: A1
    A1=0.0_8
    i=0
    ! i=1
    left=1
    right=nx-1

    do
       if(left>=right) then
          exit
       end if

       A1=(right-left)/2
       i=left+floor(A1)

       if(i==1) then
          print *, "Houston, we have a problem."
       end if

       if(array(axis, i)<x) then
          left=i+1
       else if(array(axis, i)>x) then
          right=i-1
       else
          exit
       end if
    end do

    if(i==1) then
       print *, "Houston1, we have a problem."
    end if

  end function binary_search
end module search
