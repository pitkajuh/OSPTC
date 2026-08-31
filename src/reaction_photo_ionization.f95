module reaction_photo_ionization
  use photon_type
  use tape_type
  implicit none

contains

  function photo_ionization(ph, endf, reaction_id) result(end)
    type(tape), intent(in) :: endf
    type(photon), pointer, intent(inout) :: ph
    integer, intent(in) :: reaction_id
    real(kind(1.d0)) :: ionization_energy
    logical :: end
    end=.true.
    ionization_energy=endf%mf23%ionization_energies(reaction_id)

    if(ionization_energy<ph%energy) then
       ph%energy=ph%energy-ionization_energy
       end=.false.
    ! else if(ph%energy-ionization_energy==0)
    end if
  end function photo_ionization

end module reaction_photo_ionization
