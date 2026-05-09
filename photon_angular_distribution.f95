module photon_angular_distribution
  use interpolate
  implicit none

contains

  function x(energy, mu, hc) result(r)
    ! return value in inverse Angstrom unit.
    real(kind(1.d0)), intent(in) :: energy, mu, hc
    real(kind(1.d0)) :: r
    r=(energy/hc)*((1_8-mu)/2_8)**0.5_8 ! in 1/m
    r=r/10_8**10_8 ! in 1/A
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

  function F(x1, coherent_factor, n) result(r)
    ! return coherent factor value corresponding to x1
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
    real(kind(1.d0)) :: hc, xmax, deltax2, H, T, x2_i, x2_im1
    integer :: i, j, new1
    hc=4.135667696E-15_8*299792458.0_8 ! eVs m/s
    ! xmax=energymax/hc
    ! xmax=x(2.5E6_8, -1.0_8, hc)
    ! print *, xmax



    ! do i=1, n2
    !    print *, i, coherent_factor(1, i), coherent_factor(2, i)
    ! end do



    ! deltax2=deltax

    ! do i=1, n2
    !    print *, i, coherent_factor(1, i), coherent_factor(2, i)
    ! end do


    call system('rm t1.txt')
    open(newunit=new1, file="t1.txt", status="new", action="write")


    A(1, 1)=coherent_factor(1, 1)**2
    deltax2=A(1, 1)
    A(2, 1)=0.5_8*(coherent_factor(2, 1)**2)*deltax2
    write(new1, *) A(1, 1), ";", A(2, 1)

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
       ! print *, x2_im1, x2_i, H, T, deltax2
       write(new1, *) A(1, i), ";", A(2, i)
    end do




    close(new1)
    print *, "generated"

    ! ymax=(energymax/hc)**2
    ! ymin=(energymin/hc)**2
    ! deltay=(ymax-ymin)/n
    ! energymin=50E3_8

    ! i=1
    ! A(1, i)=deltay
    ! A(2, i)=0.0_8
    ! ! print *, i,n,A(1, i), A(2, i)
    ! write(new1, *) A(1, i), ";", A(2, i)
    ! i=2
    ! A(1, i)=2.0_8*deltay
    ! A(2, i)=0.5_8*deltay*(F((n*deltay)**0.5_8, coherent_factor, n2)+&
    !      F(A(1, 2)**0.5_8, coherent_factor, n2))
    ! ! print *, i,n,A(1, i), A(2, i)
    ! write(new1, *) A(1, i), ";", A(2, i)

    ! do i=3, n
    !    A(1, i)=i*deltay
    !    A(2, i)=A(2, i-1)+deltay*F(A(1, i)**0.5_8, coherent_factor, n2)
    !    write(new1, *) A(1, i), ";", A(2, i)
    !    ! if(i<5) print *, A(1, i), A(2, i)
    !    ! print *, i,n,A(1, i), A(2, i)
    ! end do

    ! print *, A(2, 1), A(2, 2), A(2, n)
    ! print *, deltay, n

  end subroutine create_coherent
end module photon_angular_distribution
