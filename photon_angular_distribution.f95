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

  function fn(A, i) result(aa)
    real(kind(1.d0)), intent(in), allocatable :: A(:, :)
    integer, intent(in) :: i
    real(kind(1.d0)) :: aa
    aa=A(2, i)
  end function fn

  subroutine create_incoherent(incoherent_function, A, n)
    real(kind(1.d0)), intent(inout), allocatable :: A(:, :)
    real(kind(1.d0)), intent(in), allocatable :: incoherent_function(:, :)
    integer, intent(in) :: n
    integer :: i
    real(kind(1.d0)) :: deltax

    A(1, 1)=incoherent_function(1, 1)
    deltax=A(1, 1)
    A(2, 1)=0.5_8*(incoherent_function(2, 1))*deltax

    do i=2, n
       A(1, i)=incoherent_function(1, i)
       deltax=A(1, i)-A(1, i-1)
       A(2, i)=A(2, i-1)+0.5_8*(fn(incoherent_function, i-1)+fn(incoherent_function, i))*deltax
    end do
    print *, "Incoherent scattering distribution generated"
  end subroutine create_incoherent

  function F(x1, coherent_factor, n) result(r)
    ! return coherent factor value corresponding to value x1
    real(kind(1.d0)), intent(in) :: x1
    integer, intent(in) :: n
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    real(kind(1.d0)) :: r
    r=linear_interpolation(coherent_factor, x1, n, 1, 2)
  end function F

  subroutine create_coherent(coherent_factor, n, A, n2)
    real(kind(1.d0)), intent(inout), allocatable :: A(:, :)
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    integer, intent(in) :: n, n2
    real(kind(1.d0)) :: deltax2, H, T, x2_i, x2_im1
    integer :: i

    A(1, 1)=coherent_factor(1, 1)**2
    deltax2=A(1, 1)
    A(2, 1)=0.5_8*(coherent_factor(2, 1)**2)*deltax2

    do i=2, n
       A(1, i)=coherent_factor(1, i)**2
       x2_i=A(1, i)
       x2_im1=A(1, i-1)
       deltax2=x2_i-x2_im1

       H=F(x2_im1**0.5_8, coherent_factor, n2)
       T=F(x2_i**0.5_8, coherent_factor, n2)
       A(2, i)=A(2, i-1)+0.5_8*(H**2+T**2)*deltax2
    end do

    print *, "Coherent scattering distribution generated"

  end subroutine create_coherent
end module photon_angular_distribution
