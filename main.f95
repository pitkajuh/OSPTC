program main
  use read_endf
  use surface_type
  implicit none

  integer :: res

  ! Not working. Must be declared as a pointer.
  ! type(planex) :: test1
  ! call test1%create(100.0)

  class(surface), allocatable :: a1
  allocate(planex :: a1)
  call a1%create(100.0)
  print *, a1%value1
  res=read_tape('cross-sections/photoat-007_N_000.endf')
  res=read_tape('cross-sections/photoat-011_Na_000.endf')
  res=read_tape('cross-sections/photoat-022_Ti_000.endf')
  res=read_tape('cross-sections/photoat-026_Fe_000.endf')
  res=read_tape('cross-sections/photoat-053_I_000.endf')
end program main
