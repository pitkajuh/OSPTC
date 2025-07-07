

module read_endf
  implicit none
contains
  ! subroutine read_begin(tape_name)
  subroutine read_begin(z)
    implicit none

    integer, intent(in) :: z
    integer :: iostat
    character(75) :: line
    ! character :: tape_name

    ! open(1, file=tape_name, status="old", action="read", iostat=ios)

    do
       read(z, '(A75)', iostat=iostat) line
       print *, line
       exit
       if(trim(line(2:5))/="ENDF" .and. trim(line(72:75))=="0  0") exit
    end do

  end subroutine read_begin

  integer function read_tape(tape_name)
  ! subroutine read_tape(tape_name)
    implicit none

    character(*) :: tape_name
    integer :: ios, z
    ! integer :: read_tape
    ! character(75) :: line
    z=1
    print *, tape_name
    open(z, file=tape_name, status="old", action="read")
    call read_begin(z)
    close(z)
    ! read_tape=0.0
  ! end subroutine read_tape
  end function read_tape
end module read_endf

program main
  use read_endf
  implicit none

  ! character(len=5) :: line
  character(len=75) :: line
  character(len=75) :: line2
  integer(8) :: ios, z
  character(len=10) :: a1, a2, a3, a4, a5, a6, a7
  ! character(10) :: C1, C2, L1, L2, N1, N2, MAT, MF, MT, NS
  real(kind(1.d0)) :: C1, C2!, L1, L2, N1, N2, MAT, MF, MT, NS
  integer :: L1, L2, N1, N2, MAT, MF, MT, NS
  integer :: res

  res=read_tape('cross-sections/photoat-007_N_000.endf')
  ! call read_tape("cross-sections/photoat-007_N_000.endf")

  ! character(len=12) :: a1
  ! character(len=11) :: a2, a3, a4, a5, a6
  ! character(len=8) :: a7


  ! z = 7
  ! open(z, file="cross-sections/photoat-007_N_000.endf", status='old', action='read', iostat=ios)

  ! do
  !    read(z, '(A75)', iostat=ios) line
  !    if(trim(line(2:5))/="ENDF" .and. trim(line(72:75))=="0  0") exit
  ! end do

  ! do
  !    read(z, '(A75)', iostat=ios) line
  !    print *, line
  !    if (ios < 0) exit
  ! end do



!   do
!      ! read(z, "(2E11.0,4I11,I4,I2,I3,I5)") &
!      !      C1,C2,L1,L2,N1,N2,MAT,MF,MT,NS

!      ! print *, "(2E11.0,4I11,I4,I2,I3,I5)"
! ! 100  format(2E11.0,4I11,I4,I2,I3,I5)
!      ! if (ios < 0) exit

!      ! read(z, '(A75)', iostat=ios) line, line2
!      read(z, '(A75)', iostat=ios) line2
!      ! read(z, '(A22)', iostat=ios) line, line2
!      ! read(z, '(a)', iostat=ios) line
!      ! read(z, '(a)', iostat=ios) a1, a2, a3, a4, a5, a6, a7
!      if (ios < 0) exit
!      ! if (line == "23501") exit

!      print *, '@', trim(line2), '@', '@'
!      ! print *, '@', trim(line), '@', trim(line2), '@'
!      ! print *, trim(a1), a2, a3, a4, a5, a6, a7
!   end do

  ! close(z)

end program main
