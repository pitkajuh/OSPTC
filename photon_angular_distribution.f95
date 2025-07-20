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

  function x(energy, mu, hc) result(r)
    real(kind(1.d0)), intent(in) :: energy, mu, hc
    real(kind(1.d0)) :: r
    r=(energy/hc)*((1-mu)/2)**0.5
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

    ! do i=1, n1
    !    print *, i, incoherent(1, i), incoherent(2, i)
    !    ! incoherent_function_value=linear_interpolation(incoherent_function, x(incoherent(1, i), mu), n2)
    ! end do

  end subroutine create_incoherent

  function thomson(mu) result(r)
    real(kind(1.d0)), intent(in) :: mu
    real(kind(1.d0)) :: r
    r=3.14159265*2.8179403227E-15_8*2.8179403227E-15_8*(1+mu*mu)
  end function thomson

  function dsigmadmu_coherent(energy, mu, F, Fprime, Fprimeprime) result(r)
    real(kind(1.d0)), intent(in) :: energy, mu, F, Fprime, Fprimeprime
    real(kind(1.d0)) :: r, FF
    FF=F+Fprime;
    r=thomson(mu)*(FF*FF+Fprimeprime*Fprimeprime)
  end function dsigmadmu_coherent

  function d2sigmaEdmu_coherent(energy, energyprime, mu, width, F, Fprime, Fprimeprime) result(r)
    real(kind(1.d0)), intent(in) :: energy, energyprime, mu, width, F, Fprime, Fprimeprime
    real(kind(1.d0)) :: r
    r=dsigmadmu_coherent(energy, mu, F, Fprime, Fprimeprime)*dirac_delta(energyprime-energy, width)
  end function d2sigmaEdmu_coherent

  function F1(x1, coherent_factor, n) result(r)
    real(kind(1.d0)), intent(in) :: x1
    integer, intent(in) :: n
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    real(kind(1.d0)) :: r
    r=linear_interpolation(coherent_factor, x1, n)
    r=r*r
  end function F1

  subroutine create_coherent(coherent_factor, n1, elim, deltae, A)
    real(kind(1.d0)), intent(inout), allocatable :: A(:, :)
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    integer, intent(in) :: n1
    real(kind(1.d0)), intent(in) :: elim, deltae
    real(kind(1.d0)) :: deltamu, a1, e, mu, hc, deltax, x2, ymax, x1, sum1
    integer :: i, j
    hc=4.135667696e-15_8*299792458.0_8




    ymax=5000.0_8/hc
    x2=2*deltax
    i=0
    sum1=0.0

    do
       if(x2>ymax) exit
       a1=0.5*deltax*(F1(x2, coherent_factor, n1)+F1(x2-deltax, &
            coherent_factor, n1))
       sum1=sum1+a1
       x2=x2+deltax
       i=i+1
    end do





    ! e=deltae
    ! deltamu=-2.0_8/n1
    ! mu=1.0_8+deltamu
    ! deltax=x(e, mu, hc)/2
    ! ymax=5000.0_8/hc
    ! x2=2*deltax
    ! i=1
    ! sum1=0.0
    ! print *, ymax, x2
    ! do
    !    if(x2>ymax) exit
    !    a1=0.5*deltax*(F1(x2, coherent_factor, n1)+F1(x2-deltax, &
    !         coherent_factor, n1))
    !    ! print *, a1
    !    sum1=sum1+a1
    !    x2=x2+deltax
    !    i=i+1
    ! end do
    ! print *, "SUM", sum1

    ! x2=0.0
    ! i=1
    ! j=2

    ! e=deltae
    ! deltamu=-2.0_8/n1
    ! mu=1.0_8+deltamu
    ! deltax=x(e, mu, hc)/2
    ! ! deltax=100
    ! print *, "ok", n1, deltax
    ! ! A(1, 1)=0.0_8
    ! do
    !    if(mu<-1) exit
    !    ! print *, mu
    !    ! A(i, 1)=0.0_8
    !    ! print *, i
    !    do
    !       if(e>elim-deltae) exit
    !       x2=x(e, mu, hc)

    !       a1=0.5*deltax*(F1(x2, coherent_factor, n1)+F1(x2-deltax, &
    !            coherent_factor, n1))
    !       ! print *, F1(x2, coherent_factor, n1), F1(x2-deltax, coherent_factor, n1)

    !       A(i, j)=A(i, j-1)+a1
    !       ! if(A(i, j)>=sum1) print *, i, j
    !       e=e+deltae
    !       j=j+1
    !    end do
    !    print *, A(i, j-1)
    !    ! exit
    !    j=2
    !    e=deltae
    !    mu=mu+deltamu
    !    i=i+1
    ! end do



    ! print *, i, mu!, A(i-1, n1-1)

    ! ymax=(5000.0_8/(4.135667696e-15_8*299792458.0_8))
    ! x2=2*deltax
    ! i=0
    ! sum1=0.0

    ! do
    !    if(x2>ymax) exit
    !    a1=0.5*deltax*(F1(x2, coherent_factor, n1)+F1(x2-deltax, &
    !         coherent_factor, n1))
    !    sum1=sum1+a1
    !    x2=x2+deltax
    !    i=i+1
    ! end do
    ! print *, "SUM", sum1
    ! print *, ymax, x1, i, deltax
  end subroutine create_coherent

end module photon_angular_distribution
