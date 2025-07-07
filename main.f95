program main
  implicit none

  ! character(len=5) :: line
  character(len=75) :: line
  character(len=75) :: line2
  integer(8) :: ios, z
  character(len=10) :: a1, a2, a3, a4, a5, a6, a7
  ! character(10) :: C1, C2, L1, L2, N1, N2, MAT, MF, MT, NS
  real(kind(1.d0)) :: C1, C2!, L1, L2, N1, N2, MAT, MF, MT, NS
  integer :: L1, L2, N1, N2, MAT, MF, MT, NS

  ! character(len=12) :: a1
  ! character(len=11) :: a2, a3, a4, a5, a6
  ! character(len=8) :: a7
  character(4) :: endf
  z = 7
  open(z, file="cross-sections/photoat-007_N_000.endf", status='old', action='read', iostat=ios)

  do
     read(z, '(A75)', iostat=ios) line
     endf=trim(line(2:5))
     if(endf/="ENDF" .and. trim(line(72:75))=="0  0") exit
  end do

  ! do
  !    read(z, '(A75)', iostat=ios) line
  !    print *, line
  !    ! exit
  !    if (line == "23501") exit
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

  close(z)

end program main
