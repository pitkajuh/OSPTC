module photon_angular_distribution
  use interpolate
  implicit none

contains

  function dirac_delta(v, width) result(r)
    real(kind(1.d0)), intent(in) :: v, width
    real(kind(1.d0)) :: r
    r=exp(-v*v/(2*width*width))/(width*sqrt(2*3.14159265))
  end function dirac_delta

  function energyprimev(energy, mu) result(r)
    real(kind(1.d0)), intent(in) :: energy, mu
    real(kind(1.d0)) :: r
    r=energy/(1+(energy/0.51099895069E6_8)*(1-mu))
  end function energyprimev

  function dsigmadmu(energy, mu) result(r)
    real(kind(1.d0)), intent(in) :: energy, mu
    real(kind(1.d0)) :: r
    r=1
  end function dsigmadmu

  function d2sigmaEdmu(energy, energyprime, mu, width) result(r)
    real(kind(1.d0)), intent(in) :: energy, energyprime, mu, width
    real(kind(1.d0)) :: r
    r=dsigmadmu(energy, mu)*dirac_delta(energyprime-energyprimev(energy, mu), width)
  end function d2sigmaEdmu

  subroutine create_incoherent(incoherent, incoherent_function, n1, n2)
    real(kind(1.d0)), allocatable :: incoherent(:, :)
    real(kind(1.d0)), allocatable :: incoherent_function(:, :)
    integer, intent(in) :: n1, n2
    integer :: i

    do i=1, n1
       print *, i, incoherent(1, i), incoherent(2, i)
    end do

  end subroutine create_incoherent

end module photon_angular_distribution
