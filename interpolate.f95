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
    integer :: nx, ny
    real :: energy, get_mu_value
    real, dimension(nx, ny) :: array

  end function get_mu_value

end module interpolate
