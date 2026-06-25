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
     real(kind(1.d0)), allocatable :: incoherent_A(:, :)
     integer :: n, n_coherent, n_incoherent
     type(MF23) :: mf23
     type(MF27) :: mf27
  end type tape

contains

  subroutine clear_tape(this)
    type(tape), intent(inout) :: this
    call clear_mf23(this%mf23)
    call clear_mf27(this%mf27)
  end subroutine clear_tape

  subroutine read_tape(this, tape_name)
    type(tape), intent(inout) :: this
    character(*), intent(in) :: tape_name
    integer :: ios, z
    z=1

    open(z, file=tape_name, status="old", action="read", iostat=ios)
    call read_begin(this, z, ios)
    call this%mf23%create(z, ios, this%n)
    ! print *, "aoeeoaeo", this%mf23%n_ionization+5+1
    call this%mf27%create(z, ios, this%n)
    close(z)

    this%n_coherent=this%mf27%coherent_factor%n
    allocate(this%coherent_A(2, this%n_coherent))
    call create_coherent(this%mf27%coherent_factor%records, &
         this%n_coherent, this%coherent_A, this%mf27%coherent_factor%n)

    this%n_incoherent=this%mf27%incoherent_function%n
    allocate(this%incoherent_A(2, this%n_incoherent))
    call create_incoherent(this%mf27%incoherent_function%records, &
         this%incoherent_A, this%n_incoherent)

  end subroutine read_tape

  subroutine read_begin(this, z, ios)
    type(tape), intent(inout) :: this
    integer :: z, ios, i
    character(75) :: line
    real(kind(1.d0)) :: ZA, AWR
    integer :: L1, L2, N1, N2, MAT, MF, MT

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

    this%n=int(this%header(6, 4))-4
    i=1
    ! print *, "allocate", this%n

    do
       read(z, '(I36,I10,I10,I10)', iostat=ios) N1, MAT, MF, MT
       if(MT/=3) then
          exit
       else if(MAT==501 .or. MAT==516 .or. MAT==522) then
          cycle
       end if
       i=i+1
    end do

    read(z, '(I36,I10,I10,I10)', iostat=ios) N1, MAT, MF, MT

  end subroutine read_begin
end module tape_type
