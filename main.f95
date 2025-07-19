program main
  use interpolate
  use cell_type
  use tape_type
  use radionuclide_type
  use material_type
  use physics_routine
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
  ! type(steel) :: steel1
  class(material), allocatable :: steel1
  real(kind(1.d0)) :: c, ymax, Amax
  integer :: reac
  ! allocate(cylinder :: a1)
  ! call a1%create()




  allocate(steel :: steel1)
  call steel1%create()

  c=steel1%density*100*linear_interpolation(steel1%mu, 11.0_8, 37)

  ! c=linear_interpolation(steel1%mu, 12.0, 37)
  ! print *, c
  ! print *, steel1%endf%mf23%incoherent_scattering%records

  ! do i=1, int(steel1%endf%mf23%incoherent_scattering%header(1, 3))
  !    print *, i, steel1%endf%mf23%incoherent_scattering%records(1, i), steel1%endf%mf23%incoherent_scattering%records(2, i)
  ! end do

  ! do i=1, int(steel1%endf%mf23%pair_formation_elec%n)
  !    print *, i, steel1%endf%mf23%pair_formation_elec%records(1, i), steel1%endf%mf23%pair_formation_elec%records(2, i)
  ! end do

  call reaction_function(steel1%endf, rng(1.0_8, 1e9_8))

  c=5000.0_8
  ymax=(c/(4.135667696e-15_8*299792458.0_8))**2
  print *, "ymax ", ymax, ymax**0.5, c
  ! Amax=linear_interpolation2(steel1%endf%coherent_A, ymax, steel1%endf%Ax, steel1%endf%Ay)
  ! print *, "res", Amax

  ! Amax=linear_interpolation(steel1%endf%coherent_A, ymax, steel1%endf%Ay)
  ! print *, Amax

  ! print *, reac
  ! t=std_uniform_distribution()
  ! t1=std_uniform_distribution()
  ! point=coordinate(t, t, t)
  point1=coordinate(t1, t1, t1)
  a1=cylinder(10.0_8, 2.0_8, point1)

  print *, a1%surface_equation(point1)
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
