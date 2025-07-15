module random
  implicit none
contains
  function std_uniform_distribution()
    real(kind(1.d0)) :: std_uniform_distribution, rnd
    call random_seed()
    call random_number(rnd)
    std_uniform_distribution=1-rnd
  end function std_uniform_distribution

  function rng(min, max)
    real(kind(1.d0)), intent(in) :: min, max
    real(kind(1.d0)) :: rng
    rng=min+std_uniform_distribution()*(max-min)
  end function rng
end module random
