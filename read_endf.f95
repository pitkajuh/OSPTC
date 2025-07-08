module read_endf
  implicit none
contains
  subroutine read_begin(z, ios)
    implicit none

    integer :: z
    integer :: ios
    character(75) :: line
    integer :: n

    real(kind(1.d0)) :: ZA, AWR
    integer :: L1, L2, N1, N2, MAT, MF, MT

    n=0

    read(z, *, iostat=ios) line

    do
       read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       if(n==4) exit
       ! print *, ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       n=n+1
    end do

    do
       read(z, '(A71,I2,I1,I1)', iostat=ios) line, MAT, MF, MT
       ! print *, line, MAT, MF, MT
       if(MAT==0 .and. MF==0 .and. MT==0) exit
    end do

  end subroutine read_begin

  subroutine read_section(z, ios)
    implicit none

    integer :: z
    integer :: ios
    real(kind(1.d0)) :: v1, v2, v3, v4, v5, v6
    integer :: L1, L2, N1, N2, MAT, MF, MT

    do
       read(z, '(6E11.0,I4,I2,I3)', iostat=ios) v1, v2, v3, v4, v5, v6, MAT, MF, MT
       if(MT==0) exit
       ! print *, v1, v2, v3, v4, v5, v6, MAT, MF, MT
    end do

  end subroutine read_section

  subroutine read_header(z, ios)
    implicit none

    integer :: z
    integer :: ios
    integer :: n
    real(kind(1.d0)) :: ZA, AWR
    integer :: L1, L2, N1, N2, MAT, MF, MT
    n=1

    do
       read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       ! print *, ZA, AWR, L1, L2, N1, N2, MAT, MF, MT

       if(n==3) exit
       n=n+1
    end do

  end subroutine read_header

  integer function read_tape(tape_name)
    implicit none

    character(*) :: tape_name
    integer :: ios, z
    z=1

    open(z, file=tape_name, status="old", action="read", iostat=ios)
    call read_begin(z, ios)

    do
       if(ios<0) exit
       call read_header(z, ios)
       call read_section(z, ios)
       ! print *, ""
    end do

    close(z)
  end function read_tape
end module read_endf
