program main
  use interpolate
  use cell_type
  use tape_type
  use radionuclide_type
  use material_type
  implicit none

  integer :: res, i

  ! Not working. Must be declared as a pointer.
  ! type(planex) :: test1
  ! call test1%create(100.0)
  real :: t, t1, val
  type(coordinate) :: point, point1
  class(surface), allocatable :: a1
  type(tape) :: endf_tape
  type(cylinder) :: t12
  type(steel) :: steel1
  real :: c
  call steel1%create()

  c=steel1%density*100*linear_interpolation(steel1%mu, 11.0, 37)
  ! c=linear_interpolation(steel1%mu, 12.0, 37)
  ! print *, c
  ! print *, steel1%endf%mf23%incoherent_scattering%records

  ! do i=1, int(steel1%endf%mf23%incoherent_scattering%header(1, 3))
  !    print *, i, steel1%endf%mf23%incoherent_scattering%records(1, i), steel1%endf%mf23%incoherent_scattering%records(2, i)
  ! end do

  ! do i=1, int(steel1%endf%mf23%pair_formation_elec%n)
  !    print *, i, steel1%endf%mf23%pair_formation_elec%records(1, i), steel1%endf%mf23%pair_formation_elec%records(2, i)
  ! end do

  val=get_cross_section(steel1%endf, 2.5e6)

  ! steel1%get_mu_value(10)

  ! t=std_uniform_distribution()
  ! t1=std_uniform_distribution()
  ! point=coordinate(t, t, t)
  ! point1=coordinate(t1, t1, t1)
  ! call show(point)
  ! call show(point1)
  ! ! print *, point-point1
  ! point1=coordinate(t1, t1, t1)+100.0
  ! print *, point+100
  ! print *, point1

  ! allocate(planex :: a1)
  ! ! call a1%create(100.0)
  ! ! print *, a1%value1
  ! ! call a1%surface_equation(point1)
  ! deallocate(a1)



end program main
