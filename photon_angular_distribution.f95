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

  subroutine create_coherent(coherent_factor, n, elim, deltae, A)
    real(kind(1.d0)), intent(inout), allocatable :: A(:)
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    integer, intent(in) :: n
    real(kind(1.d0)), intent(in) :: elim, deltae
    real(kind(1.d0)) :: hc, deltax, x2, xmax, s1
    integer :: i, j
    hc=4.135667696e-15_8*299792458.0_8

    xmax=(elim/hc)**2
    deltax=xmax/n

    A(1)=0.5_8*deltax*(F1((n*deltax)**0.5, coherent_factor, n)+&
         F1(deltax**0.5, coherent_factor, n))

    do i=2, n
       A(i)=A(i-1)+deltax*F1((i*deltax)**0.5, coherent_factor, n)
    end do

    print *, A(1), A(2), A(n)
    print *, deltae, deltax, n
  end subroutine create_coherent
end module photon_angular_distribution
