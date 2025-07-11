program main
  ! use read_endf
  use surface_type
  use tape_type
  implicit none

  integer :: res

  ! Not working. Must be declared as a pointer.
  ! type(planex) :: test1
  ! call test1%create(100.0)
  real :: t, t1
  type(coordinate) :: point, point1
  class(surface), allocatable :: a1
  type(tape) :: endf_tape
  ! call read_tape1(endf_tape, 'cross-sections/photoat-007_N_000.endf')
  ! print *, endf_tape%header
  ! call endf_tape%read_tape1('cross-sections/photoat-007_N_000.endf')

  ! allocate(planex :: a1)
  ! call a1%create(100.0)
  ! print *, a1%value1

  call read_tape1(endf_tape, 'cross-sections/photoat-007_N_000.endf')
  print *, ""
  call read_tape1(endf_tape, 'cross-sections/photoat-011_Na_000.endf')
  print *, ""
  call read_tape1(endf_tape, 'cross-sections/photoat-022_Ti_000.endf')
  print *, ""
  call read_tape1(endf_tape, 'cross-sections/photoat-026_Fe_000.endf')
  print *, ""
  call read_tape1(endf_tape, 'cross-sections/photoat-053_I_000.endf')


  ! t=std_uniform_distribution()
  ! t1=std_uniform_distribution()
  ! point=coordinate(t, t, t)
  ! point1=coordinate(t1, t1, t1)
  ! call show(point)
  ! call show(point1)
  ! print *, point-point1
  ! point1=coordinate(t1, t1, t1)+100.0
  ! ! print *, point+100
  ! print *, point1
end program main
