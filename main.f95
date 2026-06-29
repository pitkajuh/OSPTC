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
  class(material), allocatable :: steel1
  class(material), allocatable :: nitrogen1
  type(coordinate) :: new_location, centered_at
  type(photon) :: ph
  class(radionuclide), allocatable :: co_60_source

  class(cells), allocatable :: cell_all(:)
  allocate(cell_all(2))

  allocate(cell_cylinder_truncated_z::cell_all(1)%cell_array)
  select type(cell_array => cell_all(1)%cell_array)
  class is (cell_cylinder_truncated_z)
     ! cell_array%cell_material=steel1
     call cell_array%create(0.5_8, -1.0_8, 1.0_8, 0.0_8, 0.0_8, 0.0_8)
  end select

  allocate(cell_cylinder_truncated_z::cell_all(2)%cell_array)
  select type(cell_array => cell_all(2)%cell_array)
  class is (cell_cylinder_truncated_z)
     ! cell_array%cell_material=nitrogen1
     call cell_array%create(20.0_8, -20.0_8, 20.0_8, 0.0_8, 0.0_8, 0.0_8)
  end select

  print *, size(cell_all)

  do i=1, size(cell_all)
     select type(cell_array => cell_all(i)%cell_array)
        ! print *,cell_array
        ! cell_all%cell_array(i)!%surface_cylinder%v
     end select
  end do




  ! allocate(co_60 :: co_60_source)
  ! co_60_source%activity=1E3
  ! allocate(steel :: steel1)
  ! call steel1%create()
  ! allocate(cell_cylinder_truncated_z :: source_cell)
  ! source_cell%cell_material=steel1
  ! call source_cell%create(0.5_8, -1.0_8, 1.0_8, 0.0_8, 0.0_8, 0.0_8)

  ! allocate(nitrogen :: nitrogen1)
  ! call nitrogen1%create()
  ! allocate(cell_cylinder_truncated_z :: cell2)
  ! cell2%cell_material=nitrogen1
  ! call cell2%create(20.0_8, -20.0_8, 20.0_8, 0.0_8, 0.0_8, 0.0_8)



  ! ! new_location=coordinate(0.1_8, 0.1_8, 0.1_8)
  ! ! print *, source_cell%cell_test(new_location)

  ! ! new_location=coordinate(5_8, 0.1_8, 0.1_8)
  ! ! print *, source_cell%cell_test(new_location)

  ! ! new_location=coordinate(0.1_8, 5_8, 0.1_8)
  ! ! print *, source_cell%cell_test(new_location)

  ! ! new_location=coordinate(0.1_8, 0.1_8, 5_8)
  ! ! print *, source_cell%cell_test(new_location)

  ! ! new_location=coordinate(-5_8, 0.1_8, 0.1_8)
  ! ! print *, source_cell%cell_test(new_location)

  ! ! new_location=coordinate(0.1_8, -5_8, 0.1_8)
  ! ! print *, source_cell%cell_test(new_location)

  ! ! new_location=coordinate(0.1_8, 0.1_8, -5_8)
  ! ! print *, source_cell%cell_test(new_location)

  ! ! print *, steel1%mu(2, 1)
  ! ! do i=1, nitrogen1%n
  ! !    print *, nitrogen1%mu(1, i), nitrogen1%mu(2, i), nitrogen1%mu(3, i)
  ! ! end do

  ! ! allocate(cells(2))
  ! ! allocate(cell_cylinder_truncated_z :: cells(1))
  ! ! allocate(cell_cylinder_truncated_z :: cells(2))

  ! ! do second=1, 1000
  ! !    do i=1, co_60_source%activity

  !       ph=co_60_source%pdf()
  !       ph%origin=source_cell%random_initial_position()

  !       new_location=calculate_mfp(ph, source_cell%cell_material%get_mu_value(ph%energy))
  !       ph%origin=new_location
  !       call show(new_location)
  !       print *, source_cell%cell_test(new_location)
  !       print *, cell2%cell_test(new_location)
  !       ! print *, steel1%get_mu_value(ph%energy), ph%energy/1E6_8
  !       ! call show(new_location)
  !       !  E=1E1_8
  !       call reaction_function(steel1%endf, ph)
  !       ! print *, second, i, E
  ! !    end do
  ! ! end do

  ! deallocate(co_60_source)
  ! ! call clear_material(steel1)
  ! deallocate(steel1)
  ! deallocate(nitrogen1)


  ! ! deallocate(cells(1))
  ! ! deallocate(cells(2))
  ! ! deallocate(cells)

  ! deallocate(source_cell)
  ! deallocate(cell2)
  ! ! call clear_seed()
end program main
