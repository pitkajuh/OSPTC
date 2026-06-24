module photon_angular_distribution
  use interpolate
  use constants, only: hcA
  implicit none

contains

  function x(energy, mu) result(r)
    ! return value in inverse Angstroms (A). 1 A=10^-10 m
    real(kind(1.d0)), intent(in) :: energy, mu
    real(kind(1.d0)) :: r
    r=(energy/hcA)*(0.5*(1_8-mu))**0.5_8 ! in 1/A
  end function x

  subroutine create_normalized_cdf(A, n)
    real(kind(1.d0)), allocatable :: A(:, :)
    integer, intent(in) :: n
    integer :: i
    real(kind(1.d0)) :: max_value
    max_value=A(2, n)

    do i=1, n
       A(2, i)=A(2, i)/max_value
    end do
  end subroutine create_normalized_cdf

  subroutine create_incoherent(incoherent, incoherent_function, n1, n2, A)
    real(kind(1.d0)), intent(inout), allocatable :: A(:, :)
    real(kind(1.d0)), allocatable :: incoherent(:, :)
    real(kind(1.d0)), allocatable :: incoherent_function(:, :)
    integer, intent(in) :: n1, n2
    integer :: i
    real(kind(1.d0)) :: incoherent_function_value, deltax, x_i, x_im1, H, T

    A(1, 1)=incoherent_function(1, 1)
    deltax=A(1, 1)
    A(2, 1)=0.5_8*(incoherent_function(2, 1))*deltax

    do i=2, n1
       A(1, i)=incoherent_function(1, i)
       x_i=A(1, i)
       x_im1=A(1, i-1)
       deltax=x_i-x_im1

       H=linear_interpolation(incoherent_function, x(incoherent(1, i), -1.0_8), n2, 1, 2)
       T=linear_interpolation(incoherent_function, x(incoherent(1, i), -1.0_8), n2, 1, 2)
       A(2, i)=A(2, i-1)+0.5_8*(H+T)*deltax
    end do
    print *, "Incoherent generated"
  end subroutine create_incoherent

  function F(x1, coherent_factor, n) result(r)
    ! return coherent factor value corresponding to value x1
    real(kind(1.d0)), intent(in) :: x1
    integer, intent(in) :: n
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    real(kind(1.d0)) :: r
    r=linear_interpolation(coherent_factor, x1, n, 1, 2)
  end function F

  subroutine create_coherent(coherent_factor, n, energymax, A, n2, energymin)
    real(kind(1.d0)), intent(inout), allocatable :: A(:, :)
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    real(kind(1.d0)), intent(in) :: energymax, energymin
    integer, intent(in) :: n, n2
    real(kind(1.d0)) :: xmax, deltax2, H, T, x2_i, x2_im1
    integer :: i, j, new1

    ! call system('rm t1.txt')
    ! open(newunit=new1, file="t1.txt", status="new", action="write")

    A(1, 1)=coherent_factor(1, 1)**2
    deltax2=A(1, 1)
    A(2, 1)=0.5_8*(coherent_factor(2, 1)**2)*deltax2
    ! write(new1, *) A(1, 1), ";", A(2, 1)

    ! do i=1, n2
    !    print *, coherent_factor(1, i), coherent_factor(2, i)
    ! end do

    do i=2, n
       A(1, i)=coherent_factor(1, i)**2
       x2_i=A(1, i)
       x2_im1=A(1, i-1)
       deltax2=x2_i-x2_im1

       ! print *, i, n
       H=F(x2_im1**0.5_8, coherent_factor, n2)
       T=F(x2_i**0.5_8, coherent_factor, n2)

       A(2, i)=A(2, i-1)+0.5_8*(H**2+T**2)*deltax2
       ! A(2, i)=0.5_8*(H**2+T**2)*deltax2

       ! print *, A(1, i), A(2, i)
       ! write(new1, *) A(1, i), ";", A(2, i)
    end do

    ! close(new1)
    print *, "Coherent generated"

  end subroutine create_coherent
end module photon_angular_distribution
