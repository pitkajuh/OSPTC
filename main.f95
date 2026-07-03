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
  type(photon), pointer :: ph, current_photon
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
  co_60_source%activity=1E3
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

  ! do second=1, 1000
  !    print *, second, "s"

     ! do i=1, co_60_source%activity
        allocate(ph)
        ph=co_60_source%pdf()
        ph%origin=cell_all(1)%cell_array%random_initial_position()
        ph%next_photon=>null()
        current_photon=>ph

        do while (associated(current_photon))
           call surface_tracking(cell_all, ph, 1)
           current_photon=>current_photon%next_photon
        end do


        ! do k=1, 1000

        !    mfp=calculate_mfp(ph, cell_all(1)%cell_array%cell_material &
        !         %get_mu_value(ph%energy), cell_all(1)%cell_array%cell_material% &
        !         density)
        !    cell_index=cell_search(cell_all, size(cell_all), mfp)

        !    if(cell_index>1) then
        !       ! Move photon to edge of the cells.
        !       do j=cell_index, size(cell_all)
        !          distance_to_cell=cell_all(j)%cell_array%cell_distance(ph%origin, ph%direction)
        !          print *, distance_to_cell, ph%origin, ph%direction

        !          if(cell_all(j)%cell_array%cell_material%density==cell_all(j-1) &
        !               %cell_array%cell_material%density) then
        !             ! move to mfp
        !             print *, "same material"
        !             ph%origin=mfp
        !             reaction=.true.
        !             exit
        !          else
        !             ! Add small interpolation distance in order to make sure
        !             ! that the photon ends up on the right side.
        !             distance_to_cell=distance_to_cell*1.01
        !             ph%origin=ph%origin+ph%direction*distance_to_cell
        !          end if
        !       end do
        !    else if(cell_index==0) then
        !       reaction=.false.
        !       exit
        !    else
        !       reaction=.true.
        !       exit
        !    end if

        !    if(reaction .eqv. .true.) then
        !       exit
        !    end if
        ! end do
        ! ! which index?
        ! if(reaction .eqv. .true.) then
        !    print *, j
        !    ! call reaction_function(cell_all(j)%cell_array%cell_material%endf, ph)
        ! end if
        ! if i=0, photon left geometry


        ! print *, "new", new_location%x, new_location%y, new_location%z
        ! ph%origin=new_location

        ! print *, "cc", ph%energy
        ! do
        !    ! print *, ph%energy
        !    do j=1, size(cell_all)

        !       new_location=calculate_mfp(ph, &
        !            cell_all(j)%cell_array%cell_material%get_mu_value(ph%energy), cell_all(j)%cell_array%cell_material%density)
        !       ! print *, "new", new_location%x, new_location%y, new_location%z
        !       ! ph%origin=new_location
        !       cell_hit=cell_all(j)%cell_array%cell_test(new_location)

        !       if(cell_hit .eqv. .true.) then
        !          k=j
        !          ph%origin=new_location
        !          print *, "now", new_location%x, new_location%y, new_location%z
        !          exit
        !       end if
        !    end do

        !    ! Photon did not hit any cell. It left the geometry.
        !    ! Ignore photons with energy less than 1 keV
        !    if(cell_hit .eqv. .false. .or. ph%energy<1000.0_8) then
        !       print *, "exit"
        !       exit
        !    else if(ph%energy>0) then
        !       ! continue
        !       continue_loop=.true.
        !       ! print *, "ph%energy>0"
        !    end if
        !    ! call show(new_location)
        !    ! print *, "now", new_location%x, new_location%y, new_location%z
        !    ! print *, cell_hit
        !    call reaction_function(cell_all(k)%cell_array%cell_material%endf, ph)
        ! end do
     ! end do
  ! end do

  deallocate(co_60_source)
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
