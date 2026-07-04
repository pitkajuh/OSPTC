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

  integer :: i, j, second, k, cell_index
  class(cell), allocatable :: source_cell, cell2
  class(material), allocatable :: steel1
  class(material), allocatable :: nitrogen1
  type(coordinate) :: new_location, centered_at, mfp
  type(photon), pointer :: ph, current_photon, previous_photon
  class(radionuclide), allocatable :: co_60_source
  logical :: cell_hit, continue_loop, reaction
  class(cells), allocatable :: cell_all(:)
  real(kind(1.d0)) :: distance_to_cell
  reaction=.false.
  cell_hit=.false.
  continue_loop=.false.
  k=1
  allocate(steel :: steel1)
  call steel1%create()
  allocate(nitrogen :: nitrogen1)
  call nitrogen1%create()

  allocate(cell_all(2))

  allocate(cell_cylinder_truncated_z::cell_all(1)%cell_array)
  select type(cell_array => cell_all(1)%cell_array)
  class is (cell_cylinder_truncated_z)
     cell_array%name="source"
     cell_array%cell_material=steel1
     call cell_array%create(0.1_8, -0.1_8, 0.1_8, 0.0_8, 0.0_8, 0.0_8)
  end select

  allocate(cell_cylinder_truncated_z::cell_all(2)%cell_array)
  select type(cell_array => cell_all(2)%cell_array)
  class is (cell_cylinder_truncated_z)
     cell_array%name="outside"
     cell_array%cell_material=nitrogen1
     call cell_array%create(1.0_8, -1.0_8, 1.0_8, 0.0_8, 0.0_8, 0.0_8)
  end select

  ! print *, size(cell_all)

  ! new_location=coordinate(0.1_8, 0.1_8, 0.1_8)
  ! print *, source_cell%cell_test(new_location)


  ! do i=1, size(cell_all)
  !    ! select type(cell_array => cell_all(i)%cell_array)

  !    !    ! print *,cell_array
  !    !    ! cell_all%cell_array(i)!%surface_cylinder%v
  !    ! end select
  !    print *, cell_all(i)%cell_array%name
  !    print *, cell_all(i)%cell_array%cell_test(new_location)
  ! end do




  allocate(co_60 :: co_60_source)
  co_60_source%activity=10!E3
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

  allocate(ph)
  ph%energy=1
  current_photon=>ph

  do i=2, 10
     allocate(current_photon%next_photon)
     current_photon=>current_photon%next_photon
     current_photon%energy=i
     current_photon%next_photon => null()
  end do

  previous_photon=>null()

  do while (associated(current_photon))
     print *, ph%energy
     current_photon%next_photon
  end do

  ! do second=1, 10!00
  !    print *, second, "s"

  !    do i=1, co_60_source%activity
  !       cell_index=1
  !       ph=co_60_source%pdf()
  !       ph%origin=cell_all(cell_index)%cell_array%random_initial_position()
  !       ph%next_photon=>null()
  !       current_photon=>ph

  !       do while (associated(current_photon))
  !          call surface_tracking(cell_all, current_photon, cell_index)
  !          current_photon=>current_photon%next_photon
  !       end do
  !    end do
  ! end do




  deallocate(co_60_source)
  deallocate(ph)
  ! ! call clear_material(steel1)
  ! deallocate(steel1)
  ! deallocate(nitrogen1)


  ! ! deallocate(cells(1))
  ! ! deallocate(cells(2))
  ! ! deallocate(cells)

  ! deallocate(source_cell)
  ! deallocate(cell2)
  ! ! call clear_seed()
  deallocate(cell_all)
end program main
