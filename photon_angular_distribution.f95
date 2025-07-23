module photon_angular_distribution
  use interpolate
  implicit none

contains

  function x(energy, mu, hc) result(r)
    real(kind(1.d0)), intent(in) :: energy, mu, hc
    real(kind(1.d0)) :: r
    r=(energy/hc)*((1-mu)/2)**0.5
  end function x

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

  function F1(x1, coherent_factor, n) result(r)
    real(kind(1.d0)), intent(in) :: x1
    integer, intent(in) :: n
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    real(kind(1.d0)) :: r
    r=linear_interpolation(coherent_factor, x1, n)
    r=r*r
  end function F1

  subroutine create_coherent(coherent_factor, n, energymax, A, n2)
    real(kind(1.d0)), intent(inout), allocatable :: A(:, :)
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    real(kind(1.d0)), intent(in) :: energymax
    integer, intent(in) :: n, n2
    real(kind(1.d0)) :: hc, deltay, ymax
    integer :: i, j
    hc=4.135667696e-15_8*299792458.0_8

    ymax=(energymax/hc)**2
    deltay=ymax/n
    i=1
    A(1, i)=deltay
    A(2, i)=0.0_8
    i=2
    A(1, i)=2.0_8*deltay
    A(2, i)=0.5_8*deltay*(F1((n*deltay)**0.5_8, coherent_factor, n2)+&
         F1(A(1, 2)**0.5_8, coherent_factor, n2))

    do i=3, n
       A(1, i)=i*deltay
       A(2, i)=A(2, i-1)+deltay*F1(A(1, i)**0.5_8, coherent_factor, n2)
       ! if(i<5) print *, A(1, i), A(2, i)
       ! print *, i,n,A(1, i), A(2, i)
    end do

    print *, A(2, 1), A(2, 2), A(2, n)
    print *, deltay, n

  end subroutine create_coherent
end module photon_angular_distribution
