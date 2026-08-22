module results_type
  use coordinate_type
  use photon_type

  type :: energy_distribution
     type(coordinate) :: location
     real(kind(1.d0)) :: energy
     type(energy_distribution), pointer :: next => null()
  end type energy_distribution

  type :: results
     type(energy_distribution), pointer :: energy_dist => null()
  end type results

contains

  subroutine add_result(this, ph)
    type(results), intent(inout) :: this
    type(photon), intent(in) :: ph
    type(energy_distribution), pointer :: next

    if(associated(this%energy_dist)) then
       next=>this%energy_dist

       do while(associated(next))
          if(.not. associated(next%next)) then
             allocate(next%next)
             next%next%location=ph%direction
             next%next%energy=ph%energy
             exit
          end if

          next=>next%next
       end do


    else
       allocate(this%energy_dist)
       this%energy_dist%location=ph%direction
       this%energy_dist%energy=ph%energy
    end if

  end subroutine add_result

  subroutine write_results(this)
    type(results), intent(in) :: this
    type(energy_distribution), pointer :: next, remove
    character(len=5) :: z
    character(len=10) :: t
    character(len=8) :: d
    remove=>null()
    call date_and_time(d, t, z)

    open(1, file=t, status="new")
    next=>this%energy_dist

    do while(associated(next))
       print *, next%location, next%energy

       write(1, *) next%location, next%energy
       remove=>next
       next=>next%next
       deallocate(remove)
    end do
    close(1)
  end subroutine write_results

end module results_type
