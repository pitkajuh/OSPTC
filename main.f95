program main
  ! use read_endf
  use interpolate
  use cell_type
  use tape_type
  use radionuclide_type
  use material_type
  implicit none

  integer :: res

  ! Not working. Must be declared as a pointer.
  ! type(planex) :: test1
  ! call test1%create(100.0)
  real :: t, t1
  type(coordinate) :: point, point1
  class(surface), allocatable :: a1
  type(tape) :: endf_tape
  type(cylinder) :: t12
  type(steel) :: steel1
  real :: c
  call steel1%create()

  ! c=steel1%density*100*get_mu_value(steel1%mu, 10.0, 3, 37)
  c=linear_interpolation(steel1%mu, 12.0, 37)
  print *, c
  ! steel1%get_mu_value(10)

  t=std_uniform_distribution()
  t1=std_uniform_distribution()
  point=coordinate(t, t, t)
  point1=coordinate(t1, t1, t1)
  call show(point)
  call show(point1)
  ! print *, point-point1
  point1=coordinate(t1, t1, t1)+100.0
  ! print *, point+100
  ! print *, point1

  allocate(planex :: a1)
  ! call a1%create(100.0)
  ! print *, a1%value1
  ! call a1%surface_equation(point1)
  deallocate(a1)



end program main
