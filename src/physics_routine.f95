module physics_routine
  use reaction_scattering
  use reaction_pair_production
  use reaction_photo_ionization
  implicit none

contains

  subroutine sum_cross_sections(limits, records, energy, n, total, i)
    real(kind(1.d0)), intent(in) :: energy
    real(kind(1.d0)), intent(inout) :: total
    real(kind(1.d0)), intent(inout), dimension(:) :: limits
    integer, intent(in) :: n, i
    real(kind(1.d0)), allocatable :: records(:, :)
    real(kind(1.d0)) :: r
    r=linear_interpolation(records, energy, n, 1, 2)
    limits(i)=r+limits(i-1)
    total=total+r
  end subroutine sum_cross_sections

  function select_reaction(endf, energy) result(reaction_id)
    type(tape), intent(inout) :: endf
    real(kind(1.d0)), intent(in) :: energy
    real(kind(1.d0)) :: r, total, random_value
    integer, dimension(2) :: scattering_ids
    integer, dimension(2) :: pair_production_ids
    integer :: i, reaction_id, pair_production_size, to
    real(kind(1.d0)), dimension(2) :: coherent_and_incoherent
    real(kind(1.d0)), dimension(2) :: pair_production_cross_sections
    real(kind(1.d0)), allocatable :: ionization_cross_sections(:)
    pair_production_size=0
    i=2
    r=0.0_8
    total=0.0_8
    reaction_id=0

    total=linear_interpolation(endf%mf23%coherent_scattering% &
         records, energy, endf%mf23%coherent_scattering%n, 1, 2)
    coherent_and_incoherent(1)=total
    scattering_ids(1)=502

    call sum_cross_sections(coherent_and_incoherent, endf%mf23% &
         incoherent_scattering%records, energy, &
         endf%mf23%incoherent_scattering%n, total, 2)
    scattering_ids(2)=504

    if(energy>=endf%mf23%pair_formation_elec%records(1, 1)) then
       pair_production_size=pair_production_size+1
       pair_production_ids(pair_production_size)=515
       r=linear_interpolation(endf%mf23%pair_formation_elec% &
            records, energy, endf%mf23%pair_formation_elec%n, 1, 2)
       total=total+r
       pair_production_cross_sections(pair_production_size)=total
    end if

    if(energy>=endf%mf23%pair_formation_nuc%records(1, 1)) then
       pair_production_size=pair_production_size+1
       pair_production_ids(pair_production_size)=517
       r=linear_interpolation(endf%mf23%pair_formation_nuc% &
            records, energy, endf%mf23%pair_formation_nuc%n, 1, 2)
       total=total+r
       pair_production_cross_sections(pair_production_size)=total
    end if

    ! Compare energy to largest ionization value. If it is equal or
    ! larger, all ionization reactions can be included.
    if(energy>=endf%mf23%ionization_energies(endf%mf23%n_ionization)) then
       to=endf%mf23%n_ionization
    else
       to=binary_search_1d(endf%mf23%ionization_energies, energy, &
            endf%mf23%n_ionization)
    end if

    allocate(ionization_cross_sections(to))

    do i=1, to
       r=linear_interpolation(endf%mf23%photo_ionization(endf%mf23% &
            n_ionization-i+1)%records, energy, endf%mf23% &
            photo_ionization(endf%mf23%n_ionization-i+1)%n, 1, 2)
       total=total+r
       ionization_cross_sections(i)=total
    end do

    random_value=std_uniform_distribution()

    do i=1, 2
       if(random_value<coherent_and_incoherent(i)/total) then
          deallocate(ionization_cross_sections)
          reaction_id=scattering_ids(i)
          return
       end if
    end do

    do i=1, pair_production_size
       if(random_value<pair_production_cross_sections(i)/total) then
          deallocate(ionization_cross_sections)
          reaction_id=pair_production_ids(i)
          return
       end if
    end do

    do i=1, to
       if(random_value<ionization_cross_sections(i)/total) then
          reaction_id=i
          deallocate(ionization_cross_sections)
          return
       end if
    end do
  end function select_reaction

  function reaction_function(endf, ph, mfp, energy_lost) result(end)
    type(tape), intent(inout) :: endf
    type(photon), pointer, intent(inout) :: ph
    type(coordinate), intent(in) :: mfp
    real(kind(1.d0)), intent(inout) :: energy_lost
    real(kind(1.d0)) :: energy_before
    integer :: reaction_id
    logical :: end
    end=.false.
    reaction_id=select_reaction(endf, ph%energy)

    select case (reaction_id)
    case(502)
       call coherent_scattering_reaction(ph, endf)
       energy_lost=0.0_8 ! No energy is lost in coherent/elastic scattering.
    case(504)
       energy_before=ph%energy
       call incoherent_scattering_reaction(ph, endf)
       energy_lost=energy_before-ph%energy
    case(515)
       energy_lost=ph%energy-energy_threshold_pair_elec
       call pair_production(ph, mfp)
    case(517)
       energy_lost=ph%energy-energy_threshold_pair_nuc
       call pair_production(ph, mfp)
    case default
       energy_before=ph%energy
       end=photo_ionization(ph, endf, reaction_id)
       energy_lost=energy_before-ph%energy
    end select

  end function reaction_function

end module physics_routine
