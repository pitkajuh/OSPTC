program main
  implicit none

  character(len=75) :: line
  integer(8) :: ios, unit
  character(len=10) :: a1, a2, a3, a4, a5, a6, a7
  unit = 7
  open(unit, file="cross-sections/photoat-007_N_000.endf", status='old', action='read', iostat=ios)

  do
     ! read(unit, '(A75)', iostat=ios) line
     read(unit, '(A75)', iostat=ios) a1, a2, a3, a4, a5, a6, a7
     if (ios < 0) exit

     ! print *, '@', trim(line), '@'
     print *, a1, a2, a3, a4, a5, a6, a7
  end do

  close(unit)

end program main
