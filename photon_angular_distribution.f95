module photon_angular_distribution
  use interpolate
  implicit none

contains

  function x(energy, mu, hc) result(r)
    real(kind(1.d0)), intent(in) :: energy, mu, hc
    real(kind(1.d0)) :: r
    r=(energy/hc)*((1_8-mu)/2_8)**0.5_8
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
    real(kind(1.d0)), intent(in) :: x1
    integer, intent(in) :: n
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    real(kind(1.d0)) :: r
    ! print *, "F"
    r=linear_interpolation(coherent_factor, x1, n)
    ! r=r*r
  end function F

  subroutine create_coherent(coherent_factor, n, energymax, A, n2, energymin)
    real(kind(1.d0)), intent(inout), allocatable :: A(:, :)
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    real(kind(1.d0)), intent(in) :: energymax, energymin
    integer, intent(in) :: n, n2
    real(kind(1.d0)) :: hc, xmax, xmin, deltax, deltax2, H, T
    integer :: i, j, new1
    hc=4.135667696E-15_8*299792458.0_8
    xmax=energymax/hc
    xmax=x(energymax, -1.0_8, hc)
    xmin=0.0_8
    deltax=(xmax-xmin)/n
    deltax2=deltax*deltax

    deltax=(xmax-xmin)
    deltax2=deltax*deltax/n


    deltax=(xmax**2-xmin**2)
    deltax2=deltax/n

    deltax2=xmax**2/n


    ! do i=1, n2
    !    print *, i, coherent_factor(1, i), coherent_factor(2, i)
    ! end do



    ! deltax2=deltax

    print *, deltax2, xmax/1E5_8, xmax
    ! do i=1, n2
    !    print *, i, coherent_factor(1, i), coherent_factor(2, i)
    ! end do


    call system('rm t1.txt')
    open(newunit=new1, file="t1.txt", status="new", action="write")

    A(1, 1)=0.0_8
    A(2, 1)=0.5_8*deltax2*(coherent_factor(2, 1)**2)
    H=0
    T=coherent_factor(2, 1)**2

    write(new1, *) A(1, 1), ";", A(2, 1)
    print *, 1, 0.5_8*deltax2, H, T, A(2, 1)



    A(1, 2)=deltax2
    H=coherent_factor(2, 1)
    T=F(A(1, 2)**0.5_8, coherent_factor, n2)**2
    A(2, 2)=A(2, 1)+0.5_8*deltax2*(H+T)
    ! A(2, 2)=0.5_8*deltax2*(H+T)
    write(new1, *) A(1, 2), ";", A(2, 2)
    print *, 2, A(2, 1), 0.5_8*deltax2, H, T, A(2, 2)


    ! ! do i=3, n2
    ! do i=3, n
    !    ! print *, i, n
    !    ! A(1, i)=(i-1)*deltax2
    !    ! A(2, i)=A(2, i-1)+0.5_8*deltax2*(F(A(1, i-1)**0.5_8, coherent_factor, n2)**2+&
    !    !      F(A(1, i)**0.5_8, coherent_factor, n2)**2)


    !    A(1, i)=(i-1)*deltax2

    !    if(A(1, i)**0.5_8>=1E9_8) then
    !       print *, i, (A(1, i)**0.5_8)*1E-9
    !       exit
    !    end if


    !    H=F(A(1, i-1)**0.5_8, coherent_factor, n2)**2
    !    T=F(A(1, i)**0.5_8, coherent_factor, n2)**2
    !    A(2, i)=A(2, i-1)+0.5_8*deltax2*(H+T)
    !    ! A(2, i)=0.5_8*deltax2*(H+T)
    !    ! print *, A(2, i-1), H, T
    !    ! print *, A(1, i), ";", A(2, i)
    !    write(new1, *) A(1, i), ";", A(2, i)
    !    ! print *, A(1, i), ";", A(2, i)
    !    ! print *, i, A(2, i-1), 0.5_8*deltax2, H, T, A(2, i)



    ! end do




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
