program main
  use read_endf
  use planex_type
  implicit none

  integer :: res
  type(surface) :: a3=surface(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  type(surface) :: a
  type(planex) :: a1=planex(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
  res=read_tape('cross-sections/photoat-007_N_000.endf')
  res=read_tape('cross-sections/photoat-011_Na_000.endf')
  res=read_tape('cross-sections/photoat-022_Ti_000.endf')
  res=read_tape('cross-sections/photoat-026_Fe_000.endf')
  res=read_tape('cross-sections/photoat-053_I_000.endf')
end program main
