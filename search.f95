module search
  implicit none
contains
  function binary_search(array, x, nx) result(i)
    integer :: nx, i, left, right
    real(kind(1.d0)), intent(in) :: x
    real(kind(1.d0)) :: r, A1
    real(kind(1.d0)), intent(in), dimension(:, :) :: array

    if(array(1, 1)==x) then
       i=1
    else if(array(1, nx)==x) then
       i=nx
    else
       r=0.0_8
       ! left=0
       left=1
       right=nx-1

       do
          if(left>=right) then
             exit
          end if

          A1=(right-left)/2
          i=left+floor(A1)

          if(array(1, i)<x) then
             left=i+1
          else if(array(1, i)>x) then
             right=i-1
          else
             exit
          end if
       end do
    end if
  end function binary_search
end module search
