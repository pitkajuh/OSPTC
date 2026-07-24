module random
  implicit none
  ! integer, save :: seed_n
  ! integer, allocatable, save :: seed(:)

contains

  ! subroutine clear_seed()
  !   print *, "Clear seed"
  !   deallocate(seed)
  ! end subroutine clear_seed

  ! subroutine generate_seed(n)
  !   integer :: n
  !   call random_seed(size=n)
  !   allocate(seed(n))
  !   call random_seed(get=seed)
  ! end subroutine generate_seed

  ! function rnd1() result(r)
  !   real(kind(1.d0)) :: r
  ! end function rnd1

  function std_uniform_distribution()
    real(kind(1.d0)) :: std_uniform_distribution, rnd
    call random_seed()
    call random_number(rnd)
    std_uniform_distribution=1-rnd
    ! std_uniform_distribution=0.2
  end function std_uniform_distribution

  function rng(min, max)
    real(kind(1.d0)), intent(in) :: min, max
    real(kind(1.d0)) :: rng
    rng=min+std_uniform_distribution()*(max-min)
  end function rng
end module random
