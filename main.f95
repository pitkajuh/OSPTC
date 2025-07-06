program main
  implicit none

  character(len=5) :: line
  character(len=75) :: line2
  integer(8) :: ios, unit
  character(len=10) :: a1, a2, a3, a4, a5, a6, a7
  ! character(len=12) :: a1
  ! character(len=11) :: a2, a3, a4, a5, a6
  ! character(len=8) :: a7
  unit = 7
  open(unit, file="cross-sections/photoat-007_N_000.endf", status='old', action='read', iostat=ios)

  do
     read(unit, '(A75)', iostat=ios) line
     if (line == "23501") exit
  end do



  do
     ! read(unit, '(A75)', iostat=ios) line, line2
     read(unit, '(A75)', iostat=ios) line2
     ! read(unit, '(A22)', iostat=ios) line, line2
     ! read(unit, '(a)', iostat=ios) line
     ! read(unit, '(a)', iostat=ios) a1, a2, a3, a4, a5, a6, a7
     if (ios < 0) exit
     ! if (line == "23501") exit

     print *, '@', trim(line2), '@', '@'
     ! print *, '@', trim(line), '@', trim(line2), '@'
     ! print *, trim(a1), a2, a3, a4, a5, a6, a7
  end do

  close(unit)

end program main
