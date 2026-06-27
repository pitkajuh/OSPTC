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
  class(cell), allocatable :: source_cell, cell2
  ! class(cell_cylinder_truncated_z) :: tr1
  class(material), allocatable :: steel1
  class(material), allocatable :: nitrogen1
  type(coordinate) :: new_location, centered_at
  type(photon) :: ph
  class(radionuclide), allocatable :: co_60_source
  type(cylinder) :: surface_cylinder1
  type(planez) :: wallz_negative1
  type(planez) :: wallz_positive1
  type(cylinder) :: surface_cylinder2
  type(planez) :: wallz_negative2
  type(planez) :: wallz_positive2
  centered_at=coordinate(0.0_8, 0.0_8, 0.0_8)





  ! call create_photon(ph, max_energy)
  ! allocate(cylinder :: a1)
  ! call a1%create()
  ! call generate_seed(99)
  ! class(cell), allocatable :: cells(:)

  ! allocate(co_60 :: co_60_source)
  ! co_60_source%activity=1E3
  ! allocate(steel :: steel1)
  ! call steel1%create()
  ! allocate(cell_cylinder_truncated_z :: source_cell)
  ! source_cell%cell_material=steel1
  ! call create_planez(wallz_negative1, -1.0_8)
  ! call create_planez(wallz_positive1, 1.0_8)
  ! call create_cylinder(surface_cylinder1, 0.5_8, centered_at)
  ! source_cell%surface_cylinder=surface_cylinder1
  ! source_cell%wallz_negative=wallz_negative1
  ! source_cell%wallz_positive=wallz_positive1

  ! allocate(nitrogen :: nitrogen1)
  ! call nitrogen1%create()
  ! allocate(cell_cylinder_truncated_z :: cell2)
  ! cell2%cell_material=nitrogen1
  ! call create_planez(wallz_negative2, -5.0_8)
  ! call create_planez(wallz_positive2, 5.0_8)
  ! call create_cylinder(surface_cylinder2, 5.0_8, centered_at)
  ! cell2%surface_cylinder=surface_cylinder1
  ! cell2%wallz_negative=wallz_negative1
  ! cell2%wallz_positive=wallz_positive1


  ! print *, steel1%mu(2, 1)
  ! do i=1, nitrogen1%n
  !    print *, nitrogen1%mu(1, i), nitrogen1%mu(2, i), nitrogen1%mu(3, i)
  ! end do

  ! allocate(cells(2))
  ! allocate(cell_cylinder_truncated_z :: cells(1))
  ! allocate(cell_cylinder_truncated_z :: cells(2))

  do second=1, 1000
     do i=1, co_60_source%activity

        ph=co_60_source%pdf()
        ph%origin=source_cell%random_initial_position()
        new_location=calculate_mfp(ph, source_cell%cell_material%get_mu_value(ph%energy))
        print *, steel1%get_mu_value(ph%energy), ph%energy/1E6_8
        ! call show(new_location)
        !  E=1E1_8
        call reaction_function(steel1%endf, ph)
        ! print *, second, i, E
     end do
  end do

  deallocate(co_60_source)
  ! call clear_material(steel1)
  deallocate(steel1)
  deallocate(nitrogen1)


  ! deallocate(cells(1))
  ! deallocate(cells(2))
  ! deallocate(cells)

  deallocate(source_cell)
  deallocate(cell2)
  ! call clear_seed()
end program main
