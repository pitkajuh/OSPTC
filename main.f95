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
  use results_type
  implicit none

  integer :: i, second, time_end
  class(material), pointer :: steel1, nitrogen1
  type(photon), pointer :: current_photon, ph
  class(radionuclide), allocatable :: co_60_source
  logical :: cell_hit, continue_loop, reaction, end_clause
  class(cells), allocatable :: cell_all(:)
  type(results), pointer :: statistics, next_statistics, remove
  character(len=5) :: z
  character(len=6) :: time_start
  character(len=8) :: d
  !class(material), pointer :: steel2
  ! allocate(steel :: steel2)
  ! call steel2%create()
  ! call clear_material(steel2)
  ! deallocate(steel2)


  ! call random_seed()


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
     ! cell_array%cell_material=>nitrogen1
     call cell_array%create(0.05_8, -0.05_8, 0.05_8, 0.0_8, 0.0_8, 0.0_8)
  end select

  allocate(cell_cylinder_truncated_z::cell_all(2)%cell_array)
  select type(cell_array => cell_all(2)%cell_array)
  class is (cell_cylinder_truncated_z)
     ! cell_array%name="outside"
     cell_array%cell_material=>nitrogen1
     ! cell_array%cell_material=>steel1
     call cell_array%create(1.0_8, -1.0_8, 1.0_8, 0.0_8, 0.0_8, 0.0_8)
  end select

  ! ! Create a directory for results
  ! call execute_command_line("mkdir -p results")
  ! ! print *, "aoao", stat

  ! ! if(stat==0) print *, "aoao", stat

  call date_and_time(d, time_start, z)
  call execute_command_line("mkdir -p results/"//time_start)
  allocate(co_60 :: co_60_source)
  co_60_source%activity=1E1
  time_end=10
  end_clause=.false.


  !$omp parallel private(ph, end_clause, current_photon, statistics, second)
  !$omp do
  do second=1, time_end
     allocate(statistics)
     statistics%dose=-1.0_8

     do i=1, co_60_source%activity
        allocate(ph)
        call co_60_source%pdf(ph)
        ph%id=1
        ph%origin=cell_all(1)%cell_array%random_initial_position()

        ph%next_photon=>null()
        current_photon=>ph

        do while(associated(current_photon))
           end_clause=surface_tracking(cell_all, current_photon, statistics)

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

     call write_results(statistics, second, time_start)

  end do
  !$omp end do
  !$omp end parallel


  print *, "SIMULATION END"

  print *, "ACCUMULATED DOSE", cell_all(1)%cell_array%get_dose(), cell_all(2)%cell_array%get_dose()








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





  print *, "delete source"
  deallocate(co_60_source)

  print *, "del cells"
  deallocate(cell_all)

  print *, "del steel1"
  call clear_material(steel1)
  deallocate(steel1)

  print *, "del nitrogen1"
  call clear_material(nitrogen1)
  deallocate(nitrogen1)

end program main
