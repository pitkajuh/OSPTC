module tape_type
  use random
  use search
  use file_type
  use interpolate
  use photon_angular_distribution
  implicit none

  type :: tape
     real(kind(1.d0)), dimension(6, 4) :: header
     real(kind(1.d0)), allocatable :: coherent_A(:, :)
     integer, allocatable :: sizes(:)
     integer :: n, Ax
     type(MF23) :: mf23
     type(MF27) :: mf27
  end type tape

contains

  subroutine read_tape(this, tape_name)
    type(tape) :: this
    character(*) :: tape_name
    integer :: ios, z, n1, n2, i
    real(kind(1.d0)) :: emax, emin
    z=1

    open(z, file=tape_name, status="old", action="read", iostat=ios)
    call read_begin(this, z, ios)
    call this%mf23%create(z, ios, this%sizes, this%n, 0)
    ! print *, "aoeeoaeo", this%mf23%n_ionization+5+1
    call this%mf27%create(z, ios, this%sizes, this%n, this%mf23%n_ionization+4+1)
    close(z)


    do i=1, this%n
       print *, i, this%sizes(i)
    end do


    ! ! create coherent angular distribution
    ! emax=2.5E6_8
    ! emin=1E3_8
    ! ! emax=2.1E6_8
    ! ! emax=5.1E6_8
    ! ! If n1 is changed, change it also from reaction_function
    ! ! n1=500


    ! n2=(emax/emin)**2
    ! print *, n2, emax, emin, int((emax/emin)**2)
    ! this%Ax=n2
    ! allocate(this%coherent_A(2, n2))
    ! call create_coherent(this%mf27%coherent_factor%records, &
    !      n2, emax, this%coherent_A, &
    !      this%mf27%coherent_factor%n, emin)
  end subroutine read_tape

  subroutine slice_tape(this, max_energy)
    type(tape) :: this
    integer :: slice_to, i
    real(kind(1.d0)), intent(in) :: max_energy
    slice_to=1+binary_search(this%mf23%coherent_scattering%records, max_energy, 3*this%sizes(1))
    print *, "ohcreco", slice_to, 3*this%sizes(1)
    do i=1, 3*this%sizes(1)
       print *, this%mf23%coherent_scattering%records(1, i)
    end do
  end subroutine slice_tape

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
    ! print *, "allocate", this%n
    allocate(this%sizes(this%n))

    do
       read(z, '(I36,I10,I10,I10)', iostat=ios) N1, MAT, MF, MT
       if(MT/=3) then
          exit
       else if(MAT==501 .or. MAT==516 .or. MAT==522) then
          cycle
       end if
       this%sizes(i)=MF-2
       ! print *, this%sizes(i)
       ! print *, N1, MAT, MF, MT, this%sizes(i), i
       i=i+1
    end do

    read(z, '(I36,I10,I10,I10)', iostat=ios) N1, MAT, MF, MT

  end subroutine read_begin
end module tape_type
