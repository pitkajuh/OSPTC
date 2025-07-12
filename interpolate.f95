module interpolate
  implicit none
contains
  ! function std_uniform_distribution()
  !   real :: std_uniform_distribution, rnd
  !   call random_seed()
  !   call random_number(rnd)
  !   std_uniform_distribution=1-rnd
  ! end function std_uniform_distribution

  ! function rng(min, max)
  !   real, intent(in) :: min, max
  !   real :: rng
  !   rng=min+std_uniform_distribution()*(max-min)
  ! end function rng

  function get_mu_value(array, energy, nx, ny)
    ! class(material), intent(inout) :: this
    integer :: nx, ny, i
    real :: energy, get_mu_value
    real, dimension(:, :) :: array
    print *, "get"
    do i=1, ny
       ! print *, array(1, i)
       if(array(1, i)>=energy) exit
    end do
    print *, array(1, i), array(1, i+1)
    if(array(1, i)==energy) then
       print *, "eq"
    else

    end if
  end function get_mu_value

end module interpolate
