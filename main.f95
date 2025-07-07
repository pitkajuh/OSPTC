

module read_endf
  implicit none
contains
  subroutine read_begin(z, ios)
    implicit none

    integer :: z
    integer :: ios
    character(75) :: line

    do
       read(z, '(A75)', iostat=ios) line
       ! print *, line
       if(trim(line(2:5))/="ENDF" .and. trim(line(72:75))=="0  0") exit
    end do

  end subroutine read_begin

  subroutine read_records(z, ios)
    implicit none

    integer :: z
    integer :: ios
    character(75) :: line

    do
       read(z, '(A75)', iostat=ios) line
       ! print *, line
       if(line(73:75)=="  0") exit
    end do

  end subroutine read_records

  subroutine read_header(z, ios)
    implicit none

    integer :: z
    integer :: ios
    integer :: n
    character(75) :: line

    n=1

    do
       read(z, '(A75)', iostat=ios) line
       print *, line
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
       call read_records(z, ios)
       print *, ""
    end do

    close(z)
  end function read_tape
end module read_endf

program main
  use read_endf
  implicit none

  character(len=75) :: line
  character(len=75) :: line2
  integer(8) :: ios, z
  character(len=10) :: a1, a2, a3, a4, a5, a6, a7
  ! character(10) :: C1, C2, L1, L2, N1, N2, MAT, MF, MT, NS
  real(kind(1.d0)) :: C1, C2!, L1, L2, N1, N2, MAT, MF, MT, NS
  integer :: L1, L2, N1, N2, MAT, MF, MT, NS
  integer :: res

  res=read_tape('cross-sections/photoat-007_N_000.endf')

end program main
