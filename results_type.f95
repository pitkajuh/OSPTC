module results_type
  use coordinate_type
  use photon_type
  use constants, only: elementary_charge, pi
  use omp_lib

  type :: results
     type(coordinate) :: location
     real(kind(1.d0)) :: dose
     type(results), pointer :: next => null()
  end type results

contains

  subroutine add_result(this, ph, mass_of_cell)
    type(results), pointer, intent(inout) :: this
    type(photon), pointer, intent(inout) :: ph
    real(kind(1.d0)), intent(in) :: mass_of_cell
    type(results), pointer :: next
    type(photon), pointer :: next_photon!

    if(this%dose<0) then
       next=>this
    else if(.not. associated(this%next)) then
       allocate(this%next)
       next=>this%next
    else
       next=>this%next

       do while(associated(next))
          if(.not. associated(next%next)) then
             allocate(next%next)
             next=>next%next
             exit
          end if
          next=>next%next
       end do
    end if

    next_photon=>ph

    do while(associated(next_photon))
       next%dose=next_photon%energy*elementary_charge/mass_of_cell
       next%location=next_photon%mfp

       if(.not. associated(next_photon%next_photon)) then
          exit
       end if

       next_photon=>next_photon%next_photon
       allocate(next%next)
       next=>next%next
    end do
  end subroutine add_result

  function create_file_name(time_start, time_stamp) result(file_name)
    integer, intent(in) :: time_stamp
    character(len=14), intent(in) :: time_start
    character(len=32) :: temporary
    character(len=32) :: file_name
    character(len=5) :: z
    character(len=6) :: time_start1
    character(len=8) :: d

    call date_and_time(d, time_start1, z)
    write(temporary, '(I0)') time_stamp
    temporary=trim(temporary)
    file_name=trim("results/"//time_start//"/"//temporary)

  end function create_file_name

  function create_thread_no(time_stamp) result(thread_no)
    integer, intent(in) :: time_stamp
    integer :: thread_no
    thread_no=int(time_stamp)+int(omp_get_thread_num())
  end function create_thread_no

  subroutine write_results(this, time_stamp, time_start)
    type(results), pointer, intent(inout) :: this
    type(results), pointer :: next, remove
    integer, intent(in) :: time_stamp
    character(len=14), intent(in) :: time_start
    integer :: thread_no
    character(len=32) :: file_name
    remove=>null()

    file_name=create_file_name(time_start, time_stamp)
    thread_no=create_thread_no(time_stamp)
    open(thread_no, file=file_name, status="new")
    next=>this

    do while(associated(next))
       write(thread_no, '(F0.7,",",F0.7,",",F0.7,",",G0)') next%location%x, next%location%y, next%location%z, next%dose

       if(.not. associated(next%next)) then
          exit
       end if

       remove=>next
       next=>next%next
       deallocate(remove)
    end do

    close(thread_no)
    deallocate(next)
  end subroutine write_results

end module results_type
