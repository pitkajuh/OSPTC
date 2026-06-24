program main
  use interpolate
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
  ! type(cylinder) :: t12
  class(material), allocatable :: steel1
  real(kind(1.d0)) :: E
  ! integer :: reac
  ! type(photon) :: ph
  class(radionuclide), allocatable :: co_60_source
  ! call create_photon(ph, max_energy)
  ! allocate(cylinder :: a1)
  ! call a1%create()

  allocate(co_60 :: co_60_source)
  co_60_source%activity=1E3

  ! do second=1, 10
  !    do i=1, co_60_source%activity
  !       E=co_60_source%pdf()
  !       !  E=1E1_8
  !       call reaction_function(steel1%endf, 1E1_8)
  !       print *, second, i, E
  !    end do
  ! end do

  allocate(steel :: steel1)
  call steel1%create()

  call reaction_function(steel1%endf, 2e3_8)
  call reaction_function(steel1%endf, 1e6_8)
  call reaction_function(steel1%endf, 1e5_8)
  call reaction_function(steel1%endf, 1e4_8)
  call reaction_function(steel1%endf, 5e3_8)
  call reaction_function(steel1%endf, 1e3_8)

  deallocate(co_60_source)
  call clear_material(steel1)
  deallocate(steel1)
end program main
