program main
  use interpolate
  use random
  use cell_type
  use tape_type
  use radionuclide_type
  use material_type
  use physics_routine
  use photon_type
  implicit none

  integer :: i, second
  ! real :: t, t1, val
  ! type(coordinate) :: point, point1
  ! class(surface), allocatable :: a1
  ! type(tape) :: endf_tape
  class(cell), allocatable :: t12
  class(material), allocatable :: steel1
  real(kind(1.d0)) :: E
  type(coordinate) :: test
  ! integer :: reac
  type(photon) :: ph
  class(radionuclide), allocatable :: co_60_source
  ! call create_photon(ph, max_energy)
  ! allocate(cylinder :: a1)
  ! call a1%create()
  ! call generate_seed(99)
  allocate(cell_cylinder_truncated_z :: t12)

  allocate(co_60 :: co_60_source)
  co_60_source%activity=1E3
  allocate(steel :: steel1)
  call steel1%create()

  do second=1, 1000
     do i=1, co_60_source%activity

        ph=co_60_source%pdf()
        ph%origin=t12%random_initial_position()
        !  E=1E1_8
        call reaction_function(steel1%endf, ph)
        ! print *, second, i, E
     end do
  end do

  deallocate(co_60_source)
  ! call clear_material(steel1)
  deallocate(steel1)
  deallocate(t12)
  ! call clear_seed()
end program main
