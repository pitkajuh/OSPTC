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

  subroutine create_coherent(coherent_factor, n, elim, deltae, A)
    real(kind(1.d0)), intent(inout), allocatable :: A(:)
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    integer, intent(in) :: n
    real(kind(1.d0)), intent(in) :: elim, deltae
    real(kind(1.d0)) :: hc, deltax, x2, xmax, s1
    integer :: i, j
    hc=4.135667696e-15_8*299792458.0_8

    ! deltax=deltae/hc
    xmax=elim/hc
    deltax=xmax/n
    x2=2*deltax
    i=2

    ! print *, n, int(xmax/deltax), int(elim/deltae)
    ! A(1)=0.0_8

    ! do i=2, int(elim/deltae)
    ! do i=2, n
    !    ! deltax=x2-(x2-deltax)
    !    A(i)=A(i-1)+0.5_8*deltax*(F1(x2, coherent_factor, n)+ &
    !         F1(x2-deltax, coherent_factor, n))
    !    x2=x2+deltax
    ! end do

    A(1)=0.5_8*deltax*(F1(n*deltax, coherent_factor, n)+&
         F1(deltax, coherent_factor, n))

    do i=2, n
       A(i)=A(i-1)+deltax*F1(i*deltax, coherent_factor, n)
    end do

    print *, A(1), A(2), A(n)
    print *, deltae, deltax, n

    ! deltax=3.14_8/100
    ! x2=deltax
    ! s1=0.0_8
    ! do i=2, 100
    !    ! deltax=x2-(x2-deltax)
    !    ! A(i)=A(i-1)+0.5_8*deltax*(sin(x2)+ &
    !    !      sin(x2-deltax))
    !    s1=s1+0.5_8*deltax*(sin(x2)+ &
    !         sin(x2-deltax))
    !    ! print *, x2, s1
    !    x2=x2+deltax
    ! end do

    deltax=3.14_8/100
    x2=deltax
    s1=0.5_8*(sin(deltax)+sin(3.14_8))*deltax

    do i=2, 100-1
       ! deltax=x2-(x2-deltax)
       ! A(i)=A(i-1)+0.5_8*deltax*(sin(x2)+ &
       !      sin(x2-deltax))
       s1=s1+deltax*sin(i*deltax)
       ! s1=s1+0.5_8*deltax*(sin(x2)+ &
       !      sin(x2-deltax))
       ! print *, i*deltax, s1
       x2=x2+deltax
    end do

    ! open(1, file="test.txt", status="new")

    ! do i=1, int(elim/deltae)
    !    ! print *, i, A(i)
    !    ! write(1, *) array(1, i), array(2, i)
    !    write(1, *) A(i)
    ! end do

  end subroutine create_coherent

  ! subroutine create_coherent(coherent_factor, n, elim, deltae, A)
  !   real(kind(1.d0)), intent(inout), allocatable :: A(:, :)
  !   real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
  !   integer, intent(in) :: n
  !   real(kind(1.d0)), intent(in) :: elim, deltae
  !   real(kind(1.d0)) :: deltamu, a1, e, mu, hc, deltax, x2, ymax, x1, sum1
  !   integer :: i, j
  !   hc=4.135667696e-15_8*299792458.0_8


  !   e=0.0_8
  !   i=1
  !   j=2
  !   e=deltae
  !   deltamu=-2.0_8/n
  !   mu=1.0_8+deltamu
  !   ! deltax=x(deltae, -1*deltamu, hc)
  !   deltax=1000
  !   print *, n, 2.0_8/n, deltamu, elim/deltae, deltae/hc
  !   allocate(A(int(-1*deltamu), int(elim/deltae)))

  !   do
  !      if(mu<-1) exit

  !      do
  !         if(e>elim) exit
  !         x2=x(e, mu, hc)
  !         a1=0.5*deltax*(F1(x2, coherent_factor, n)+F1(x2-deltax, &
  !              coherent_factor, n))
  !         ! if(x2>1E9_8) exit
  !         ! print *, e, x2, deltax, a1, mu, i, j
  !         ! print *, a1
  !         j=j+1
  !         e=e+deltae
  !      end do
  !      ! exit
  !      e=deltae
  !      mu=mu+deltamu
  !      j=2
  !      i=i+1
  !   end do


  !   ! do
  !   !    if(x2>ymax) exit
  !   !    a1=0.5*deltax*(F1(x2, coherent_factor, n)+F1(x2-deltax, &
  !   !         coherent_factor, n))
  !   !    ! sum1=sum1+a1
  !   !    A(i)=A(i-1)+a1
  !   !    ! print *, A(i)
  !   !    x2=x2+deltax
  !   !    i=i+1

  !   ! end do
  !   ! print *, i, A(i-1)
  !   ! print *, A(1), A(2), A(i-1)
  !   ! deltax=deltae/hc
  !   ! ymax=5000.0_8/hc
  !   ! x2=2*deltax
  !   ! ! i=2
  !   ! sum1=0.0_8
  !   ! print *, ymax, deltax, ymax/deltax, A(2)
  !   ! allocate(A(int(ymax/deltax)))

  !   ! do
  !   !    if(x2>ymax) exit
  !   !    a1=0.5*deltax*(F1(x2, coherent_factor, n)+F1(x2-deltax, &
  !   !         coherent_factor, n))
  !   !    sum1=sum1+a1
  !   !    ! A(i)=A(i-1)+a1
  !   !    x2=x2+deltax
  !   !    ! i=i+1
  !   ! end do
  !   ! print *, i, sum1
  ! end subroutine create_coherent

end module photon_angular_distribution
