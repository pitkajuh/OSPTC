module physics_routine
  use random
  use tape_type
  use interpolate
  use cell_type
  use photon_type
  use search
  use photon_angular_distribution
  use results_type
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

  function sample_coherent_scattering_angle(n, energy, A) result(mu)
    real(kind(1.d0)), intent(in), allocatable :: A(:, :)
    real(kind(1.d0)), intent(in) :: energy
    integer, intent(in) :: n
    real(kind(1.d0)) :: Amax, Avalue, rand, mu, x2, x2max
    integer :: i
    x2max=x(energy, -1.0_8)**2
    mu=0.0_8
    Amax=linear_interpolation(A, x2max, n, 1, 2)
    i=0

    do
       rand=std_uniform_distribution()
       Avalue=rand*Amax
       i=binary_search(A, Avalue, n, 2)
       x2=A(1, i)+(Avalue-A(2, i))*((A(1, i+1)-A(1, i))/(A(2, i+1)-A(2, i)))
       mu=1-2*x2*x2max

       if(rand<0.5*(1+mu)) exit
    end do
  end function sample_coherent_scattering_angle

  function kahns_method(a) result(mu)
    real(kind(1.d0)), intent(in) :: a
    real(kind(1.d0)) :: p1, p2, p3, y, mu
    y=0.0_8
    mu=0.0_8

    do
       p1=std_uniform_distribution()
       p2=std_uniform_distribution()
       p3=std_uniform_distribution()

       if(p1<(2*a+1)/(2*a+9)) then
          y=1+2*a*p2

          if(p3<4*(1/y-y**(-2))) then
             mu=1-2*p2
             exit
          end if
       else
          y=(2*a+1)/(1+2*a*p2)
          mu=1-(y-1)/a

          if(p3<(0.5*(mu*mu+1/y))) then
             exit
          end if
       end if
    end do
  end function kahns_method

  function sample_incoherent_scattering_angle(n, energy, A) result(mu)
    real(kind(1.d0)), intent(in), allocatable :: A(:, :)
    real(kind(1.d0)), intent(in) :: energy
    integer, intent(in) :: n
    real(kind(1.d0)) :: Amax, Avalue, rand, mu, x_value, xmax, a1, compare
    xmax=x(energy, -1.0_8)
    mu=0.0_8
    rand=0.0_8
    Avalue=0.0_8
    x_value=0.0_8
    Amax=1/linear_interpolation(A, xmax, n, 1, 2)
    a1=energy/electron_mass

    do
       mu=kahns_method(a1)
       rand=std_uniform_distribution()
       x_value=x(energy, mu)
       Avalue=linear_interpolation(A, x_value, n, 1, 2)
       compare=Avalue*Amax
       if(rand<=compare) exit
    end do
  end function sample_incoherent_scattering_angle

  subroutine incoherent_scattering_reaction(ph, endf)
    type(tape), intent(inout) :: endf
    type(photon), pointer, intent(inout) :: ph
    real(kind(1.d0)) :: mu
    mu=sample_incoherent_scattering_angle(endf%n_incoherent, ph%energy, endf%incoherent_A)
    ph%energy=ph%energy/(1+(ph%energy/electron_mass)*(1-mu))
    ph%direction=create_unit_vector(ph%direction*mu)
  end subroutine incoherent_scattering_reaction

  subroutine coherent_scattering_reaction(ph, endf)
    type(tape), intent(inout) :: endf
    type(photon), pointer, intent(inout) :: ph
    real(kind(1.d0)) :: mu
    mu=sample_coherent_scattering_angle(endf%n_coherent, ph%energy, endf%coherent_A)
    ph%direction=create_unit_vector(ph%direction*mu)
  end subroutine coherent_scattering_reaction

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

  function surface_tracking(cell_all, ph, cell_from, statistics) result(end_tracking)
    class(cells), intent(inout), allocatable :: cell_all(:)
    type(photon), pointer, intent(inout) :: ph
    integer, intent(inout) :: cell_from
    type(photon), pointer :: temp
    type(results), pointer, intent(inout) :: statistics
    integer :: cell_index, k, j
    logical :: end_tracking, has_next, has_previous
    real(kind(1.d0)) :: distance_to_cell, energy_lost
    temp=>null()
    end_tracking=.false.
    has_next=.false.
    has_previous=.false.
    distance_to_cell=0.0_8
    cell_index=1
    k=1
    j=1
    energy_lost=0.0_8

    do k=1, 1000
       ! Ignore photons with energy less than 1 keV.
       if(ph%energy<1000) then
          end_tracking=.true.
          exit
       end if

       ph%mfp=calculate_mfp(ph, cell_all(cell_from)%cell_array%cell_material &
            %get_mu_value(ph%energy), cell_all(cell_from)%cell_array% &
            cell_material% density)
       cell_index=cell_search(cell_all, size(cell_all), ph%mfp, ph%energy)

       if(cell_index>1) then
          do j=cell_index, size(cell_all)
             distance_to_cell=cell_all(j)%cell_array% &
                  cell_distance(ph%origin, ph%direction)

             if(cell_all(j)%cell_array%cell_material%density==cell_all(j-1) &
                  %cell_array%cell_material%density) then
                ! move to mfp
                print *, "same material"
                call add_result(statistics, ph)
                ph%origin=ph%mfp
                end_tracking=.false.
                end_tracking=reaction_function(cell_all(j)%cell_array%cell_material% &
                     endf, ph, ph%mfp, energy_lost)
                cell_all(j)%cell_array%accumulated_energy=cell_all(j)%cell_array%accumulated_energy+energy_lost
                exit
             else
                ! Add small interpolation distance in order to make sure
                ! that the photon ends up on the right side.
                distance_to_cell=distance_to_cell*1.01
                ph%origin=ph%origin+ph%direction*distance_to_cell
                cell_from=j
             end if
          end do
       else if(cell_index==0) then
          end_tracking=.true.
          exit
       else
          ! Reaction happens at the source.
          call add_result(statistics, ph)

          end_tracking=reaction_function(cell_all(cell_index)%cell_array% &
               cell_material%endf, ph, ph%mfp, energy_lost)
          cell_all(cell_index)%cell_array%accumulated_energy=cell_all(cell_index)%cell_array%accumulated_energy+energy_lost
          exit
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

end module physics_routine
