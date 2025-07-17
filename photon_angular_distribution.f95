module photon_angular_distribution
  use interpolate
  implicit none

contains

  function dirac_delta(v, width) result(r)
    real(kind(1.d0)), intent(in) :: v, width
    real(kind(1.d0)) :: r
    ! Dirac delta is approximated as Gaussian function
    r=exp(-v*v/(2*width*width))/(width*sqrt(2*3.14159265))
  end function dirac_delta

  function x(energy, mu) result(r)
    real(kind(1.d0)), intent(in) :: energy, mu
    real(kind(1.d0)) :: r
    r=(energy/(4.135667696e-15_8*299792458_8))*sqrt((1-mu)/2)
  end function x

  function energyprimev(energy, mu) result(r)
    real(kind(1.d0)), intent(in) :: energy, mu
    real(kind(1.d0)) :: r
    r=energy/(1+(energy/0.51099895069e6_8)*(1-mu))
  end function energyprimev

  function klein_nishina(energy, mu) result(r)
    real(kind(1.d0)), intent(in) :: energy, mu
    real(kind(1.d0)) :: r, energyprime, k, kprime, kk, muprime
    energyprime=energyprimev(energy, mu)
    k=energy/0.51099895069E+06_8
    kprime=energyprime/0.51099895069E+06_8
    kk=kprime/k
    muprime=1-mu
    r=3.14159265_8*2.8179403227E-15_8*2.8179403227E-15_8*kk*kk*(1+mu*mu+k*kprime*muprime*muprime)
  end function klein_nishina

  function dsigmadmu(energy, mu, v) result(r)
    real(kind(1.d0)), intent(in) :: energy, mu, v
    real(kind(1.d0)) :: r
    r=v*klein_nishina(energy, mu)
  end function dsigmadmu

  function d2sigmaEdmu(energy, energyprime, mu, width, v) result(r)
    real(kind(1.d0)), intent(in) :: energy, energyprime, mu, width, v
    real(kind(1.d0)) :: r
    r=dsigmadmu(energy, mu, v)*dirac_delta(energyprime-energyprimev(energy, mu), width)
  end function d2sigmaEdmu

  subroutine create_incoherent(incoherent, incoherent_function, n1, n2)
    real(kind(1.d0)), allocatable :: incoherent(:, :)
    real(kind(1.d0)), allocatable :: incoherent_function(:, :)
    integer, intent(in) :: n1, n2
    integer :: i
    real(kind(1.d0)) :: incoherent_function_value


    do i=1, n1
       print *, i, incoherent(1, i), incoherent(2, i)
       ! incoherent_function_value=linear_interpolation(incoherent_function, x(incoherent(1, i), mu), n2)
    end do

  end subroutine create_incoherent

end module photon_angular_distribution
