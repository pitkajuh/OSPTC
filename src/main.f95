program main
  use omp_lib
  use interpolate
  use random
  use cell_type
  use tape_type
  use radionuclide_type
  use material_type
  use physics_routine
  use surface_tracking_algorithm
  use photon_type
  use results_type
  use post_processing
  implicit none

  character(len=32) :: arg
  integer :: i, second, time_end, j, k, stat, io
  class(material), pointer :: steel1, nitrogen1
  type(photon), pointer :: current_photon, ph
  class(radionuclide), allocatable :: co_60_source
  logical :: end_clause
  class(cells), allocatable :: cell_all(:)
  type(results), pointer :: statistics
  real(kind(1.d0)) :: x0, x1, y0, y1, z0, z1
  character(len=5) :: z
  character(len=6) :: time_start
  character(len=8) :: d
  character(len=14) :: new_time
  character(len=32) :: results_directory
  integer :: file_name
  !class(material), pointer :: steel2
  ! allocate(steel :: steel2)
  ! call steel2%create()
  ! call clear_material(steel2)
  ! deallocate(steel2)


  ! call random_seed()

  do j=1, command_argument_count()
     call get_command_argument(j, arg)
     select case(arg)
     case("run")
        allocate(steel :: steel1)
        call steel1%create()
        allocate(nitrogen :: nitrogen1)
        call nitrogen1%create()

        allocate(cell_all(2))

        allocate(cell_box_3d::cell_all(1)%cell_array)
        select type(cell_array=>cell_all(1)%cell_array)
        class is(cell_box_3d)
           ! cell_array%name="source"
           cell_array%cell_material=>steel1
           ! cell_array%cell_material=>nitrogen1
           call cell_array%create(-0.05_8, 0.05_8, -0.05_8, 0.05_8, -0.05_8, 0.05_8)
        end select

        allocate(cell_cylinder_truncated_z::cell_all(2)%cell_array)
        select type(cell_array=>cell_all(2)%cell_array)
        class is(cell_cylinder_truncated_z)
           ! cell_array%name="outside"
           cell_array%cell_material=>nitrogen1
           ! cell_array%cell_material=>steel1
           call cell_array%create(1.0_8, -1.0_8, 1.0_8, 0.0_8, 0.0_8, 0.0_8)
        end select

        ! Create a directory for results
        call date_and_time(d, time_start, z)
        new_time=d//time_start
        call execute_command_line("mkdir -p results/"//new_time)
        allocate(co_60 :: co_60_source)
        co_60_source%activity=1E1
        time_end=10
        end_clause=.false.

        !$omp parallel private(ph, end_clause, current_photon, statistics)
        !$omp do
        do second=1, time_end
           allocate(statistics)
           statistics%energy=-1.0_8

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

           call write_results(statistics, second, new_time)

        end do
        !$omp end do
        !$omp end parallel


        print *, "SIMULATION END"
        ! print *, "ACCUMULATED DOSE", cell_all(1)%cell_array%get_dose(), cell_all(2)%cell_array%get_dose()








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
        exit
     case("postproc")
        print *, "Starting post processing"
        call get_command_argument(j+1, arg)
        results_directory=arg

        call execute_command_line("ls -1 "//results_directory//"> result_files.txt", exitstat=stat)

        if(stat/=0) then
           print *, "Error reading"
        end if

        open(newunit=k, file="result_files.txt", status="old", action="read", iostat=io)

        time_end=1

        print *, file_name
        do
           read(k, '(I10)', iostat=io) file_name

           if(file_name>time_end) then
              time_end=file_name
           end if

           exit
           if(is_iostat_end(io)) exit
        end do
        close(k)
        call execute_command_line("rm result_files.txt")

        print *, results_directory

        x0=-1.0_8
        x1=1.0_8
        y0=-1.0_8
        y1=1.0_8
        z0=-1.0_8
        z1=1.0_8
        print *, "process results"
        call post_process_results(results_directory, time_end, x0, x1, y0, y1, z0, z1)
        exit
     case default
        print *, "Unknow command: ", arg
     end select
  end do


end program main
