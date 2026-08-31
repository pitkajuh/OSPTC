module reaction_pair_production
  use photon_type
  use constants, only: electron_mass, energy_threshold_pair_nuc, energy_threshold_pair_elec
  implicit none

contains

  subroutine create_annihilation_photon(photon1, direction, origin, id)
    type(photon), pointer, intent(inout) :: photon1
    type(coordinate), intent(in) :: direction, origin
    integer, intent(in) :: id
    photon1%energy=electron_mass
    photon1%origin=origin
    photon1%direction=create_unit_vector(direction)
    photon1%id=id
  end subroutine create_annihilation_photon

  subroutine pair_production(ph, mfp)
    type(photon), pointer, intent(inout) :: ph
    type(coordinate), intent(in) :: mfp
    type(photon), pointer :: left, right, current_photon, temporary_photon
    allocate(left)
    allocate(right)

    ! Create photon going left
    call create_annihilation_photon(left, multiply_scalar(mfp, -1.0_8), &
         mfp, ph%id+1)
    ! Connect to head photon
    ph%next_photon=>left

    ! Create photon going right
    call create_annihilation_photon(right, mfp, mfp, ph%id+2)
    right%previous_photon=>left
    left%next_photon=>right

    temporary_photon=>ph
    ph=>ph%next_photon

    if(associated(temporary_photon)) then
       deallocate(temporary_photon)
    end if

    current_photon=>ph
  end subroutine pair_production

end module reaction_pair_production
