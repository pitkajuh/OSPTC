module results_type
  use coordinate_type
  use photon_type
  use omp_lib

  ! type :: energy_distribution
  !    type(coordinate) :: location
  !    real(kind(1.d0)) :: energy
  !    type(energy_distribution), pointer :: next => null()

  ! end type energy_distribution

  type :: results
     ! type(energy_distribution), pointer :: energy_dist => null()
     type(coordinate) :: location
     real(kind(1.d0)) :: energy
     type(results), pointer :: next => null()
  end type results

contains

  subroutine add_result(this, ph)
    type(results), pointer, intent(inout) :: this
    type(photon), pointer, intent(inout) :: ph
    type(results), pointer :: next
    type(photon), pointer :: next_photon!, delete_photon
    logical :: nothing
    nothing=.false.
    ! print *, "add_result"
    ! if(.not. associated(ph)) then
    !    print *, "help"
    ! end if
    ! next=>null()
    ! if(.not. associated(this%energy_dist)) then
    !    print *, "help2"
    ! end if
    if(.not. associated(this)) then
       print *, "STOP"
       error stop
    end if
    ! print *, "ADDING", ph%energy, ph%mfp
    ! if(.not. associated(this%next) .and. this%energy<0) then
    if(this%energy<0) then
       ! print *, "this%energy<0", this%energy
       next=>this
    ! end if
       ! else
    else if(.not. associated(this%next)) then
       ! print *, ".not. associated(this%next)"
       allocate(this%next)
       next=>this%next
    else
       ! print *, "ELSE", associated(this%next)
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
       next%energy=next_photon%energy
       next%location=next_photon%mfp
       ! print *, next_photon%energy/1000, next_photon%mfp, loc(next)
       if(.not. associated(next_photon%next_photon)) then
          ! print *, "EXIT"
          ! allocate(next%next)
          exit
       end if

       next_photon=>next_photon%next_photon
       allocate(next%next)
       next=>next%next
    end do

       ! if(this%energy .neqv. 0) then
       !    this%energy=ph%energy
    !    this%location=ph%mfp

    !    if(.not. associated(this%next)) then
    !       allocate(this%next)
    !       next=>this%next

    !       do while(associated(next))
    !          if(.not. associated(next%next)) then
    !             print *, ".not. associated(next%next)"
    !             print *, ph%energy
    !             allocate(next%next)
    !             next%next%location=ph%mfp
    !             next%next%energy=ph%energy
    !             nothing=.true.
    !             exit
    !          end if

    !          next=>next%next
    !       end do
    !    end if



    ! else if(.not. associated(this%next)) then
    !    allocate(this%next)
    !    this%next%energy=ph%energy
    !    this%next%location=ph%mfp
    ! else


    ! end if



    ! ! ! if(associated(this)) then
    ! ! if(this%energy>0) then
    ! !    next=>this
    ! !    print *, "alloc", loc(next)
    ! !    do while(associated(next))
    ! !       if(.not. associated(next%next)) then
    ! !          print *, ".not. associated(next%next)"
    ! !          print *, ph%energy
    ! !          allocate(next%next)
    ! !          next%next%location=ph%mfp
    ! !          next%next%energy=ph%energy
    ! !          nothing=.true.
    ! !          exit
    ! !       end if

    ! !       next=>next%next
    ! !    end do
    ! !    print *, "alloc end", nothing
    ! ! else
    ! !    print *, "unallocated"
    ! !    print *, ph%energy
    ! !    allocate(this)
    ! !    this%location=ph%mfp
    ! !    this%energy=ph%energy
    ! !    print *, "anallo end"
    ! ! end if
    ! print *, "add result end"
  end subroutine add_result

  function create_file_name(time_start, time_stamp) result(file_name)
    integer, intent(in) :: time_stamp
    character(len=6), intent(in) :: time_start
    character(len=32) :: temporary
    character(len=32) :: file_name
    character(len=5) :: z
    character(len=6) :: time_start1
    character(len=8) :: d
    call date_and_time(d, time_start1, z)
    write(temporary, '(I0)') time_stamp
    tread_id=time_stamp+omp_get_thread_num()
    temporary=trim(temporary)
    file_name="results/"//time_start//"/"//temporary
    file_name=trim(file_name)
    print *, "file name", file_name
  end function create_file_name

  ! function create_file(time_start, time_stamp) result(thread_id)
  !   integer :: i, tread_id
  !   character(len=32) :: temporary, file_name
  !   tread_id=int(time_stamp)+omp_get_thread_num()
  !   file_name=create_file_name(time_start, time_stamp)
  !   open(tread_id, file=file_name, status="new")
  ! end function create_file

  subroutine write_results(this, time_stamp, time_start)
    type(results), pointer, intent(inout) :: this
    type(results), pointer :: next, remove
    integer, intent(in) :: time_stamp
    character(len=6), intent(in) :: time_start
    integer :: i, tread_id
    character(len=32) :: temporary, file_name
    character(len=5) :: z
    character(len=6) :: time_start1
    character(len=8) :: d
    remove=>null()
    i=1
    ! print *, "write"
    ! file_name=create_file_name(time_start, time_stamp)

    call date_and_time(d, time_start1, z)
    write(temporary, '(I0)') time_stamp
    temporary=trim(temporary)
    tread_id=int(time_stamp)+int(omp_get_thread_num())

    open(tread_id, file="results/"//time_start//"/"//temporary, status="new")
    ! open(tread_id, file=file_name, status="new")

    next=>this

    do while(associated(next))

       write(tread_id, *) next%location, next%energy
       ! print *, next%location, next%energy

       if(.not. associated(next%next)) then
          ! print *, "(associated(next%next)"
          exit
       end if

       remove=>next
       next=>next%next
       ! remove%next=>null()
       ! print *, "remove", loc(remove)
       deallocate(remove)
       ! remove=>null()

    end do

    close(tread_id)
    deallocate(next)
  end subroutine write_results

end module results_type
