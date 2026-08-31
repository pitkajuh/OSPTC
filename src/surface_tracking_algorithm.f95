module surface_tracking_algorithm
  use photon_type
  use cell_type
  use results_type
  use physics_routine
  implicit none

contains

  function surface_tracking(cell_all, ph, statistics) result(end_tracking)
    class(cells), intent(inout), allocatable :: cell_all(:)
    type(photon), pointer, intent(inout) :: ph
    type(photon), pointer :: temp
    type(results), pointer, intent(inout) :: statistics
    integer :: i, cell_index, cell_from
    logical :: end_tracking, has_next, has_previous
    real(kind(1.d0)) :: distance_to_cell, energy_lost
    cell_from=1
    temp=>null()
    end_tracking=.false.
    has_next=.false.
    has_previous=.false.
    distance_to_cell=0.0_8
    cell_index=1
    energy_lost=0.0_8

    ! 1000 is just a threshold for preventing infinite loops.
    do i=1, 1000
       ! Ignore photons with energy less than 1 keV.
       if(ph%energy<1E3) then
          end_tracking=.true.
          exit
       end if

       if(end_tracking) then
          exit
       end if

       ph%mfp=calculate_mfp(ph, cell_all(cell_from)%cell_array%cell_material &
            %get_mu_value(ph%energy), cell_all(cell_from)%cell_array% &
            cell_material%density)
       cell_index=cell_search(cell_all, size(cell_all), ph%mfp, ph%energy)

       if(cell_index==0) then
          ! Photon left the geometry without reacting.
          end_tracking=.true.
          exit
       else if(cell_index-cell_from>1) then
          print *, "Photon going over multiple cells. What to do now?"
          error stop
       else if(cell_index>0) then
          distance_to_cell=cell_all(cell_index)%cell_array% &
               cell_distance(ph%origin, ph%direction)

          if(length(ph%mfp)<distance_to_cell) then
             call add_result(statistics, ph, cell_all(cell_index)%cell_array%mass, cell_index)
             ph%origin=ph%mfp
             end_tracking=reaction_function(cell_all(cell_index)%cell_array%cell_material% &
                  endf, ph, ph%mfp, energy_lost)
             !omp reduction(+:cell_all(cell_index)%cell_array%accumulated_energy)
             cell_all(cell_index)%cell_array%accumulated_energy=cell_all(cell_index)% &
                  cell_array%accumulated_energy+energy_lost
          else
             ! Add small interpolation distance in order to make sure
             ! that the photon ends up on the right side.
             distance_to_cell=distance_to_cell*1.01
             ph%origin=ph%origin+ph%direction*distance_to_cell
             cell_from=cell_index
          end if
       end if
    end do

    if(end_tracking .eqv. .true.) then
       has_next=associated(ph%next_photon)
       has_previous=associated(ph%previous_photon)

       if(has_next .and. .not. has_previous) then
          ! Current photon is head.
          temp=>ph
          ph=>ph%next_photon
          deallocate(temp)
       else if(.not. has_next .and. has_previous) then
          ! Current photon is last.
          temp=>ph
          ph%previous_photon=>null()
          deallocate(temp)
       else if(has_previous .and. has_next) then
          ! Current photon is in the middle.
          temp=>ph
          ph%previous_photon%next_photon=>ph%next_photon
          ph%next_photon%previous_photon=>ph%previous_photon
          deallocate(temp)
       else
          deallocate(ph)
          ph=>null()
       end if
    end if

  end function surface_tracking
end module surface_tracking_algorithm
