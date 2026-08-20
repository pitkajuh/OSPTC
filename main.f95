program main
  use omp_lib
  use interpolate
  use random
  use cell_type
  use tape_type
  use radionuclide_type
  use material_type
  use physics_routine
  use photon_type
  implicit none

  integer, allocatable :: seed1(:)
  ! integer, dimension(100) :: ix
  integer :: n, time, count1, i
  integer :: second, cell_index, seed
  class(material), pointer :: steel1, nitrogen1, steel2
  type(photon), pointer :: current_photon, ph, temp
  class(radionuclide), allocatable :: co_60_source
  logical :: cell_hit, continue_loop, reaction, end_clause
  class(cells), allocatable :: cell_all(:)
  ! allocate(steel :: steel2)
  ! call steel2%create()
  ! call clear_material(steel2)
  ! deallocate(steel2)

  reaction=.false.
  cell_hit=.false.
  continue_loop=.false.
  allocate(steel :: steel1)
  call steel1%create()
  allocate(nitrogen :: nitrogen1)
  call nitrogen1%create()

  allocate(cell_all(2))

  allocate(cell_cylinder_truncated_z::cell_all(1)%cell_array)
  select type(cell_array => cell_all(1)%cell_array)
  class is (cell_cylinder_truncated_z)
     ! cell_array%name="source"
     cell_array%cell_material=>steel1
     call cell_array%create(0.1_8, -0.1_8, 0.1_8, 0.0_8, 0.0_8, 0.0_8)
  end select

  allocate(cell_cylinder_truncated_z::cell_all(2)%cell_array)
  select type(cell_array => cell_all(2)%cell_array)
  class is (cell_cylinder_truncated_z)
     ! cell_array%name="outside"
     cell_array%cell_material=>nitrogen1
     ! cell_array%cell_material=>steel1
     call cell_array%create(1.0_8, -1.0_8, 1.0_8, 0.0_8, 0.0_8, 0.0_8)
  end select


  allocate(co_60 :: co_60_source)
  co_60_source%activity=1E2
  end_clause=.false.
  !$omp parallel private(ph, cell_index, end_clause, current_photon)
  !$omp do
  do second=1, 10
     do i=1, co_60_source%activity
        allocate(ph)
        cell_index=1
        call co_60_source%pdf(ph)
        ph%id=1
        ph%origin=cell_all(cell_index)%cell_array%random_initial_position()

        ph%next_photon=>null()
        current_photon=>ph

        do while(associated(current_photon))
           end_clause=surface_tracking(cell_all, current_photon, cell_index)

           if(end_clause .eqv. .false.) then
              cycle
           else if(end_clause .eqv. .true.) then
              if(associated(current_photon)) then
                 deallocate(current_photon)
              end if
              exit
           end if
           current_photon=>current_photon%next_photon
        end do
     end do
  end do
  !$emp end do
  !$omp end parallel

  print *, "END"
  print *, "delete source"
  deallocate(co_60_source)

  ! current_photon=>ph

  ! do while(associated(current_photon))
  !    temp=>current_photon

  !    if(associated(temp)) then
  !       print *, "delete", temp%id
  !       deallocate(temp)
  !    end if
  !    current_photon=>current_photon%next_photon
  ! end do

  ! if(associated(ph)) then
  !    print *, "main, deleting", ph%id
  !     deallocate(ph)
  ! end if



  ! do i=1, size(cell_all)
  !    ! deallocate(cell_all(i))
  !    select type(cell_array => cell_all(i)%cell_array)
  !       deallocate(cell_array)
  !       deallocate(cell_array(i))
  !       deallocate(cell_all(i))
  ! !    !    print *,cell_all(i)%name
  ! !    !    ! cell_all%cell_array(i)!%surface_cylinder%v
  !    end select
  ! !    ! print *, cell_all(i)%cell_array%name
  ! !    ! deallocate(cell_all(i)%cell_array%cell_material%mu)
  ! !    ! deallocate(cell_all(i)%cell_array%cell_material%endf%coherent_A)
  ! !    ! deallocate(cell_all(i)%cell_array%cell_material%endf%mf23%photo_ionization)
  ! !    ! call clear_mf23(cell_all(i)%cell_array%cell_material%endf%mf23)
  ! !    ! call clear_mf27(cell_all(i)%cell_array%cell_material%endf%mf27)
  ! !    ! ! print *, cell_all(i)%cell_array%cell_test(new_location)
  ! end do

  ! print *, "del steel1"
  ! call clear_material(steel1)
  ! deallocate(steel1)

  ! print *, "del nitrogen1"
  ! call clear_material(nitrogen1)
  ! deallocate(nitrogen1)
  ! cell_all(2)%cell_array%cell_material=>null()
  print *, "del cells"
  deallocate(cell_all)

  print *, "del steel1"
  call clear_material(steel1)
  deallocate(steel1)

  print *, "del nitrogen1"
  call clear_material(nitrogen1)
  deallocate(nitrogen1)

  print *, "END"
end program main
