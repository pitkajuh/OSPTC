module tape_type
  use file_type
  implicit none

  type :: tape
     real, dimension(6, 4) :: header
  end type tape

contains

  subroutine read_tape1(this, tape_name)
    type(tape) :: this
    character(*) :: tape_name
    integer :: ios, z
    z=1

    open(z, file=tape_name, status="old", action="read", iostat=ios)
    call read_begin1(this, z, ios)

    ! do
    !    if(ios<0) exit
    !    call read_file_header(z, ios)
    !    call read_section(z, ios)
    ! !    ! print *, ""
    ! end do

    close(z)
  end subroutine read_tape1

  subroutine read_begin1(this, z, ios)
    implicit none
    type(tape) :: this
    integer :: z
    integer :: ios
    character(75) :: line
    integer :: n

    real(kind(1.d0)) :: ZA, AWR
    integer :: L1, L2, N1, N2, MAT, MF, MT

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
       ! print *, ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       n=n+1
    end do

    do
       read(z, '(A65,I2)', iostat=ios) line, N1
       if(N1==3) exit
    end do
    ! n=0
    do
       read(z, '(A31,I10,I10,I10,I6,I1,I2,I2)', iostat=ios) line, N1, MAT, MF, MT, N2, L1, L2

       ! if(MAT==0 .and. MF==0 .and. MT==0) exit
       if(MT/=3) exit
       print *, N1, MAT, MF, MT, N2, L1, L2
       ! if(n==20) exit
       ! n=n+1
    end do

    ! do
    !    read(z, '(A71,I2,I1,I1)', iostat=ios) line, MAT, MF, MT
    !    ! print *, line, MAT, MF, MT
    !    if(MAT==0 .and. MF==0 .and. MT==0) exit
    ! end do

  end subroutine read_begin1

end module tape_type
