module tape_type
  use file_type
  implicit none

  type :: tape
     real, dimension(6, 4) :: header
     type(MF23) :: mf23
     type(MF27) :: mf27
  end type tape

contains

  subroutine read_tape1(this, tape_name)
    type(tape) :: this
    character(*) :: tape_name
    integer :: ios, z, MF, MT
    z=1

    open(z, file=tape_name, status="old", action="read", iostat=ios)
    call read_begin1(this, z, ios)

    call this%mf23%create(z, ios, MF, MT)
    call this%mf27%create(z, ios, MF, MT)
    ! Get MF23
    ! do
       ! call read_file_header(this%mf23, z, ios, MF, MT)
    !    if(MT==0 .and. MF==0) exit
    !    call read_section(this%mf23, z, ios, MF, MT)
    !    print *, ""
    ! end do

    ! Get MF27
    ! do
    !    call read_file_header(this%mf27, z, ios, MF, MT)
    !    if(MT==0 .and. MF==0) exit
    !    call read_section(this%mf27, z, ios, MF, MT)
    !    print *, ""
    ! end do

    close(z)
  end subroutine read_tape1

  subroutine read_begin1(this, z, ios)
    type(tape) :: this
    integer :: z
    integer :: ios
    character(75) :: line
    integer :: n

    real(kind(1.d0)) :: ZA, AWR
    integer :: L1, L2, N1, N2, MAT, MF, MT, MT1, MF1

    n=1

    read(z, *, iostat=ios) line

    do
       read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       this%header(1, n)=ZA
       this%header(2, n)=AWR
       this%header(3, n)=L1
       this%header(4, n)=L2
       this%header(5, n)=N1
       this%header(6, n)=N2
       if(n==4) exit
       n=n+1
    end do

    do
       read(z, '(A65,I1)', iostat=ios) line, N1
       if(N1==3)  exit
    end do

    n=0

    do
       read(z, '(I36,I10,I10,I10)', iostat=ios) N1, MAT, MF, MT
       if(MT/=3) exit
       print *, N1, MAT, MF, MT
    end do

    read(z, '(I36,I10,I10,I10)', iostat=ios) N1, MAT, MF, MT

  end subroutine read_begin1
end module tape_type
