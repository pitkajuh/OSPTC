module tape_type
  use random
  use file_type
  use interpolate
  implicit none

  type :: tape
     real, dimension(6, 4) :: header
     integer, allocatable :: sizes(:)
     integer :: n
     type(MF23) :: mf23
     type(MF27) :: mf27
  end type tape

contains

  function get_cross_section(this, energy) result(r)
    type(tape), intent(inout) :: this
    real, intent(in) :: energy
    real :: r, total
    integer :: i, n_not_zero
    real(kind(1.d0)) :: random_value
    real, dimension(4+this%mf23%n_ionization) :: v
    real(kind(1d0)), dimension(4+this%mf23%n_ionization+1) :: limits
    limits(1)=0.0
    n_not_zero=0
    r=linear_interpolation(this%mf23%coherent_scattering%records, energy, this%mf23%coherent_scattering%n)
    v(1)=r
    limits(2)=r
    total=total+r
    n_not_zero=n_not_zero+1
    print *, "incoherent", r

    r=linear_interpolation(this%mf23%incoherent_scattering%records, energy, this%mf23%incoherent_scattering%n)
    v(2)=r
    limits(3)=limits(2)+r
    total=total+r
    n_not_zero=n_not_zero+1
    print *, "coherent", r

    r=linear_interpolation(this%mf23%pair_formation_elec%records, energy, this%mf23%pair_formation_elec%n)
    v(3)=r
    total=total+r
    if(r>0.0) n_not_zero=n_not_zero+1
    limits(4)=limits(3)+r
    print *, "pair form elec", r

    r=linear_interpolation(this%mf23%pair_formation_nuc%records, energy, this%mf23%pair_formation_nuc%n)
    v(4)=r
    total=total+r
    if(r>0.0) n_not_zero=n_not_zero+1
    limits(5)=limits(4)+r
    print *, "pair form nuc", r

    do i=1, this%mf23%n_ionization
       r=linear_interpolation(this%mf23%photo_ionization(i)%records, energy, this%mf23%photo_ionization(i)%n)
       v(4+i)=r
       total=total+r
       if(r>0.0) n_not_zero=n_not_zero+1
       limits(5+i)=limits(4+i)+r
       print *, "ionization", i+4, r
    end do

    ! total=v(i)

    random_value=std_uniform_distribution()
    ! 0.93298842883496458
    print *, "random", random_value

    do i=1, 4+this%mf23%n_ionization+1
       ! print *, v(i)/total, limits(i)
       print *, i, limits(i)/total
       ! if(v(i)>0.0) total=total+r
    end do

    do i=2, 4+this%mf23%n_ionization+1
       if(random_value>=limits(i-1)/total .and. random_value<limits(i)/total .and. limits(i-1)/total/=limits(i)) then
          print *, i
       end if
       ! print *, v(i)/total, limits(i)
       ! print *, limits(i)/total
       ! if(v(i)>0.0) total=total+r
    end do

  end function get_cross_section

  subroutine read_tape(this, tape_name)
    type(tape) :: this
    character(*) :: tape_name
    integer :: ios, z
    z=1

    open(z, file=tape_name, status="old", action="read", iostat=ios)
    call read_begin(this, z, ios)
    call this%mf23%create(z, ios, this%sizes, this%n)
    call this%mf27%create(z, ios, this%sizes, this%n)
    close(z)
  end subroutine read_tape

  subroutine read_begin(this, z, ios)
    type(tape) :: this
    integer :: z, ios, i
    character(75) :: line
    real(kind(1.d0)) :: ZA, AWR
    integer :: L1, L2, N1, N2, MAT, MF, MT, MT1, MF1, n

    read(z, *, iostat=ios) line

    do i=1, 4
       read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       this%header(1, i)=ZA
       this%header(2, i)=AWR
       this%header(3, i)=L1
       this%header(4, i)=L2
       this%header(5, i)=N1
       this%header(6, i)=N2
    end do

    do
       read(z, '(A65,I1)', iostat=ios) line, N1
       if(N1==3)  exit
    end do

    this%n=this%header(6, 4)-4
    i=1
    allocate(this%sizes(this%n))

    do
       read(z, '(I36,I10,I10,I10)', iostat=ios) N1, MAT, MF, MT
       if(MT/=3) then
          exit
       else if(MAT==501 .or. MAT==516 .or. MAT==522) then
          cycle
       end if
       this%sizes(i)=MF-2
       ! print *, N1, MAT, MF, MT, this%sizes(i), i
       i=i+1
    end do

    read(z, '(I36,I10,I10,I10)', iostat=ios) N1, MAT, MF, MT

  end subroutine read_begin
end module tape_type
