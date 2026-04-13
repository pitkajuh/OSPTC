module interpolate
  implicit none
contains

  function linear_interpolation(array, x, nx) result(r)
    integer :: nx, i, L1, R1
    real(kind(1.d0)), intent(in) :: x
    real(kind(1.d0)) :: r, A1
    real(kind(1.d0)), intent(in), dimension(:, :) :: array

    r=0.0_8
    L1=0
    R1=nx-1
    ! print *, "AOE"
    do
       if(L1>=R1) then
          exit
       end if

       A1=(R1-L1)/2
       i=L1+floor(A1)

       if(array(1, i)<x) then
          L1=i+1
       else if(array(1, i)>x) then
          R1=i-1
       else
          exit
       end if
    end do


    ! do i=1, nx
    !    ! print *, i
    !    if(array(1, i)>=x) exit
    ! end do


    ! if(i>=nx) then
    !    i=i-1
    !    r=array(2, i-1)+(array(2, i)-array(2, i-1))*((x-array(1, i-1))/(array(1, i)-array(1, i-1)))
    ! else


       r=array(2, i-1)+(x-array(1, i-1))*(array(2, i)-array(2, i-1))/(array(1, i)-array(1, i-1))


    ! endif
  end function linear_interpolation
end module interpolate
