module tape_type
  use random
  use file_type
  use interpolate
  implicit none

  type :: tape
     real(kind(1.d0)), dimension(6, 4) :: header
     integer, allocatable :: sizes(:)
     integer :: n
     type(MF23) :: mf23
     type(MF27) :: mf27
  end type tape

contains

  subroutine sum1(limits, records, energy, n, total, i)
    real(kind(1.d0)), intent(in) :: energy
    real(kind(1.d0)) :: r, total
    real(kind(1.d0)), dimension(:) :: limits
    integer :: n, i
    real(kind(1.d0)), allocatable :: records(:, :)
    r=linear_interpolation(records, energy, n)
    limits(i)=r+limits(i-1)
    total=total+r
  end subroutine sum1

  function get_cross_section(this, energy) result(r)
    type(tape), intent(inout) :: this
    real(kind(1.d0)), intent(in) :: energy
    real(kind(1.d0)) :: r, total
    real(kind(1.d0)) :: random_value
    real(kind(1.d0)), dimension(4+this%mf23%n_ionization+1) :: limits
    integer :: i

    r=0
    limits(1)=0.0
    total=0

    call sum1(limits, this%mf23%coherent_scattering%records, energy, this%mf23%coherent_scattering%n, total, 2)

    call sum1(limits, this%mf23%incoherent_scattering%records, energy, this%mf23%incoherent_scattering%n, total, 3)

    call sum1(limits, this%mf23%pair_formation_elec%records, energy, this%mf23%pair_formation_elec%n, total, 4)

    call sum1(limits, this%mf23%pair_formation_nuc%records, energy, this%mf23%pair_formation_nuc%n, total, 5)

    do i=1, this%mf23%n_ionization
       call sum1(limits, this%mf23%photo_ionization(i)%records, energy, this%mf23%photo_ionization(i)%n, total, 5+i)
    end do
    print *, total
    random_value=std_uniform_distribution()

    do i=2, 4+this%mf23%n_ionization
       if(random_value<limits(i)/total) exit
    end do

    print *, i, limits(i-1)/total, random_value, limits(i)/total
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
