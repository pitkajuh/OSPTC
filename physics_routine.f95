module physics_routine
  use random
  use tape_type
  use interpolate
  use cell_type
  use photon_type
  use search
  use photon_angular_distribution
  use constants, only: electron_mass
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
    ! print *, "pair"

    ! Create photon going left
    call create_annihilation_photon(left, multiply_scalar(mfp, -1.0_8), &
         mfp, ph%id+1)
    ! Connect to head photon
    ! left%previous_photon=>ph
    ph%next_photon=>left

    ! Create photon going right
    call create_annihilation_photon(right, mfp, mfp, ph%id+2)
    right%previous_photon=>left
    left%next_photon=>right


    temporary_photon=>ph
    ph=>ph%next_photon
    if(associated(temporary_photon)) then
       ! print *, "deleting photon", temporary_photon%id
       deallocate(temporary_photon)
    end if


    current_photon=>ph




    ! do while(associated(current_photon))
    !    ! print *, current_photon%energy
    !    if(.not. associated(current_photon%next_photon)) then
    !       ! print *, "aocrahoec"
    !       allocate(current_photon%next_photon)
    !       current_photon%next_photon=>left
    !       ! print *, "at", current_photon%next_photon%energy
    !       ! current_photon%next_photon%next_photon=>right
    !       exit
    !    end if
    !    current_photon=>current_photon%next_photon
    ! end do

    ! current_photon=>ph

    ! do while(associated(current_photon))
    !    print *, current_photon%energy, current_photon%id
    !    ! if(associated(current_photon%next_photon)) then
    !    !    current_photon%next_photon=>left
    !    !    exit
    !    ! end if
    !    current_photon=>current_photon%next_photon
    ! end do


    ! ph%next_photon
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
    ! print *, "sample"
    rand=0.0_8
    Avalue=0.0_8
    x_value=0.0_8
    Amax=1/linear_interpolation(A, xmax, n, 1, 2)
    ! print *, n, energy
    a1=energy/electron_mass

    do
       ! print *, 1
       mu=kahns_method(a1)
       ! print *, 2
       rand=std_uniform_distribution()
       ! print *, 3
       x_value=x(energy, mu)
       ! print *, 4
       Avalue=linear_interpolation(A, x_value, n, 1, 2)
       ! print *, 5
       ! print *, i, Avalue/Amax
       compare=Avalue*Amax
       if(rand<=compare) exit
       ! print *, 6
    end do
    ! print *, i, mu
  end function sample_incoherent_scattering_angle

  subroutine incoherent_scattering_reaction(ph, endf)
    type(tape), intent(inout) :: endf
    type(photon), pointer, intent(inout) :: ph
    real(kind(1.d0)) :: mu
    mu=sample_incoherent_scattering_angle(endf%n_incoherent, ph%energy, endf%incoherent_A)
    ! print *, "angle"
    ph%energy=ph%energy/(1+(ph%energy/electron_mass)*(1-mu))
    ! print *,"e"
    ph%direction=create_unit_vector(ph%direction*mu)
    ! print *, "dir"
  end subroutine incoherent_scattering_reaction

  subroutine coherent_scattering_reaction(ph, endf)
    type(tape), intent(inout) :: endf
    type(photon), pointer, intent(inout) :: ph
    real(kind(1.d0)) :: mu
    mu=sample_coherent_scattering_angle(endf%n_coherent, ph%energy, endf%coherent_A)
    ph%direction=create_unit_vector(ph%direction*mu)
    ! print *, length(ph%direction)
  end subroutine coherent_scattering_reaction

  subroutine sum_cross_sections(limits, records, energy, n, total, i)
    real(kind(1.d0)), intent(in) :: energy
    real(kind(1.d0)), intent(inout) :: total
    real(kind(1.d0)), intent(inout), dimension(:) :: limits
    integer, intent(in) :: n, i
    real(kind(1.d0)), allocatable :: records(:, :)
    real(kind(1.d0)) :: r
    ! print *, "sum"
    r=linear_interpolation(records, energy, n, 1, 2)
    limits(i)=r+limits(i-1)
    total=total+r
  end subroutine sum_cross_sections

  ! subroutine select_possible_reactions(endf, energy)
  !   ! Select which reactions are possible with the given energy.
  !   ! Scattering reactions are always possible. Pair production
  !   ! and ionization depends on the energy
  !   type(tape), intent(in) :: endf
  !   real(kind(1.d0)), intent(in) :: energy
  !   integer :: i, size
  !   ! Start from number 2 because there are always at least
  !   ! two reactions (coherent and incoherent scattering.)
  !   size=2

  !   if(energy>=2*electron_mass) then
  !      ! Include pair production in nuclear field. ID=5
  !      size=size+1
  !      i=1
  !   if(energy>=4*electron_mass) then
  !      ! Include pair production in electron field. ID=4
  !      size=size+1
  !      i=1
  !   do i=1, endf%mf23%n_ionization
  !      if(energy>=endf%mf23%photo_ionization(i)%header(1, 2)) then
  !         size=size+1
  !         print *, endf%mf23%photo_ionization(i)%header(1, 2)
  !      end if
  !   end do
  ! end subroutine select_possible_reactions

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
    ! print *, "TATOL", total
    coherent_and_incoherent(1)=total
    scattering_ids(1)=502
    call sum_cross_sections(coherent_and_incoherent, endf%mf23% &
         incoherent_scattering%records, energy, &
         endf%mf23%incoherent_scattering%n, total, 2)
    scattering_ids(2)=504

    if(energy>=endf%mf23%pair_formation_elec%records(1, 1)) then
       ! print *, "energy>=4*electron_mass"
       pair_production_size=pair_production_size+1
       pair_production_ids(pair_production_size)=515
       r=linear_interpolation(endf%mf23%pair_formation_elec% &
            records, energy, endf%mf23%pair_formation_elec%n, 1, 2)
       ! print *, total, r
       total=total+r
       pair_production_cross_sections(pair_production_size)=total
    end if

    if(energy>=endf%mf23%pair_formation_nuc%records(1, 1)) then
       ! print *, "energy>=2*electron_mass"
       pair_production_size=pair_production_size+1
       pair_production_ids(pair_production_size)=517
       r=linear_interpolation(endf%mf23%pair_formation_nuc% &
            records, energy, endf%mf23%pair_formation_nuc%n, 1, 2)
       ! print *, total, r
       total=total+r
       pair_production_cross_sections(pair_production_size)=total
    end if

    ! Compare energy to largest ionization value. If it is equal or
    ! larger, all ionization reactions can be included.
    ! print *, energy, endf%mf23%ionization_energies(endf%mf23%n_ionization), endf%mf23%ionization_energies(1)
    if(energy>=endf%mf23%ionization_energies(endf%mf23%n_ionization)) then
       to=endf%mf23%n_ionization
    else
       ! print *, "not all"
       to=binary_search_1d(endf%mf23%ionization_energies, energy, &
            endf%mf23%n_ionization)
    end if

    ! do i=1, endf%mf23%n_ionization
    !    ! print *, "energy", endf%mf23%ionization_energies(i), energy
    ! end do


    allocate(ionization_cross_sections(to))
    ! if(to<endf%mf23%n_ionization) then
    !    print *, "start ion", energy, endf%mf23%ionization_energies(to), &
    !         endf%mf23%ionization_energies(endf%mf23%n_ionization-to), to, endf%mf23%n_ionization, &
    !         endf%mf23%ionization_energies(to+1)
    ! end if
    do i=1, to
       ! r=linear_interpolation(endf%mf23%photo_ionization(i)%records, &
       !      energy, endf%mf23%photo_ionization(i)%n, 1, 2)
       r=linear_interpolation(endf%mf23%photo_ionization(endf%mf23%n_ionization-i+1)%records, &
            energy, endf%mf23%photo_ionization(endf%mf23%n_ionization-i+1)%n, 1, 2)
       ! print *, "total, r", total, r, to
       ! print *, ""
       ! print *, "get ion", total, r, energy, endf%mf23%ionization_energies(i)
       total=total+r
       ionization_cross_sections(i)=total
    end do

    random_value=std_uniform_distribution()

    do i=1, 2
       ! print *, "scatter", random_value, coherent_and_incoherent(i)/total,  coherent_and_incoherent(i), total
       if(random_value<coherent_and_incoherent(i)/total) then
          deallocate(ionization_cross_sections)
          reaction_id=scattering_ids(i)
          return
       end if
    end do

    do i=1, pair_production_size
       ! print *, "pair", random_value, pair_production_cross_sections(i)/total, pair_production_cross_sections(i), total
       if(random_value<pair_production_cross_sections(i)/total) then
          deallocate(ionization_cross_sections)
          reaction_id=pair_production_ids(i)
          return
       end if
    end do

    do i=1, to
       ! print *, "ion", random_value, ionization_cross_sections(i)/total, ionization_cross_sections(i), total
       if(random_value<ionization_cross_sections(i)/total) then
          reaction_id=i
          deallocate(ionization_cross_sections)
          return
       end if
    end do
  end function select_reaction

  function reaction_function(endf, ph, mfp) result(end)
    type(tape), intent(inout) :: endf
    type(photon), pointer, intent(inout) :: ph
    type(coordinate), intent(in) :: mfp
    integer :: reaction_id
    logical :: end
    end=.false.
    reaction_id=select_reaction(endf, ph%energy)

    select case (reaction_id)
    case(502)
       ! print *, "coherent scattering", ph%energy
       call coherent_scattering_reaction(ph, endf)
    case(504)
       ! print *, "incoherent scattering", ph%energy
       call incoherent_scattering_reaction(ph, endf)
    case(515)
       ! print *, "pair formation in electric field", ph%energy
       call pair_production(ph, mfp)
    case(517)
       ! print *, "pair formation in nuclear field", ph%energy
       call pair_production(ph, mfp)
    case default
       ! print *, "ionization", ph%energy
       end=photo_ionization(ph, endf, reaction_id)
    end select

  end function reaction_function

  function surface_tracking(cell_all, ph, cell_from) result(end)
    class(cells), intent(inout), allocatable :: cell_all(:)
    type(photon), pointer, intent(inout) :: ph
    integer, intent(inout) :: cell_from
    type(photon), pointer :: temp
    integer :: cell_index, k, j
    logical :: end, has_next, has_previous
    real(kind(1.d0)) :: distance_to_cell
    type(coordinate) :: mfp
    temp=>null()
    end=.false.
    has_next=.false.
    has_previous=.false.
    mfp=coordinate(0.0_8, 0.0_8, 0.0_8)
    distance_to_cell=0.0_8
    cell_index=1
    k=1
    j=1

    ! print *, "PHOTON", ph%id

    do k=1, 1000
       ! Ignore photons with energy less than 1 keV.
       if(ph%energy<1000) then
          end=.true.
          exit
       end if

       mfp=calculate_mfp(ph, cell_all(cell_from)%cell_array%cell_material &
            %get_mu_value(ph%energy), cell_all(cell_from)%cell_array% &
            cell_material% density)
       cell_index=cell_search(cell_all, size(cell_all), mfp)

       if(cell_index>1) then
          do j=cell_index, size(cell_all)
             distance_to_cell=cell_all(j)%cell_array% &
                  cell_distance(ph%origin, ph%direction)

             if(cell_all(j)%cell_array%cell_material%density==cell_all(j-1) &
                  %cell_array%cell_material%density) then
                ! move to mfp
                print *, "same material"
                ph%origin=mfp
                end=.false.
                end=reaction_function(cell_all(j)%cell_array%cell_material% &
                     endf, ph, mfp)
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
          ! remove photon from linked list
          ! has_next=associated(ph%next_photon)
          ! has_previous=associated(ph%previous_photon)
          ! print *, "exited, want to delete", ph%id

          ! ! temp=>ph
          ! if(has_next .and. .not. has_previous) then
          !    ! Current photon is head.
          !    print *, "AAA .not. has_previous"
          !    temp=>ph
          !    ph=>ph%next_photon
          !    deallocate(temp)
          ! else if(.not. has_next .and. has_previous) then
          !    ! Current photon is last.
          !    print *, "AAA .not. has_next"
          !    print *, "previous photon is", ph%previous_photon%id
          !    temp=>ph
          !    ! ph=>ph%previous_photon
          !    ph%previous_photon=>null()
          !    deallocate(temp)
          ! else if(has_previous .and. has_next) then
          !    ! Current photon is in the middle.
          !    print *, "AAA, middle", has_previous, has_next
          !    temp=>ph
          !    print *, "first", ph%previous_photon%next_photon%id, ph%next_photon%id
          !    ph%previous_photon%next_photon=>ph%next_photon
          !    print *, "second", ph%next_photon%previous_photon%id, ph%previous_photon%id
          !    ph%next_photon%previous_photon=>ph%previous_photon

          !    deallocate(temp)
          ! else
          !    print *, "deleting11", ph%id
          !    deallocate(ph)
          !    ph=>null()
          !    print *, "deleted11"
          ! !    print *, "what happened?"
          ! !    error stop
          ! end if

          end=.true.
          exit
       else
          end=.false.
          ! Reaction happens at the source.
          ! print *, "at source", cell_from, cell_index
          end=reaction_function(cell_all(cell_index)%cell_array% &
               cell_material%endf, ph, mfp)

          ! print *, "----- origin end1"
          ! ph%origin=mfp
          ! call show(mfp)
          ! print *, "-----"
          exit
       end if
       ! print *, ph%energy
    end do

    if(end .eqv. .true.) then
       has_next=associated(ph%next_photon)
       has_previous=associated(ph%previous_photon)
       ! print *, "exited, want to delete", ph%id

       ! temp=>ph
       if(has_next .and. .not. has_previous) then
          ! Current photon is head.
          ! print *, "AAA .not. has_previous"
          temp=>ph
          ph=>ph%next_photon
          deallocate(temp)
       else if(.not. has_next .and. has_previous) then
          ! Current photon is last.
          ! print *, "AAA .not. has_next"
          ! print *, "previous photon is", ph%previous_photon%id
          temp=>ph
          ! ph=>ph%previous_photon
          ph%previous_photon=>null()
          deallocate(temp)
       else if(has_previous .and. has_next) then
          ! Current photon is in the middle.
          ! print *, "AAA, middle", has_previous, has_next
          temp=>ph
          ! print *, "first", ph%previous_photon%next_photon%id, ph%next_photon%id
          ph%previous_photon%next_photon=>ph%next_photon
          ! print *, "second", ph%next_photon%previous_photon%id, ph%previous_photon%id
          ph%next_photon%previous_photon=>ph%previous_photon

          deallocate(temp)
       else
          ! print *, "deleting11", ph%id
          deallocate(ph)
          ph=>null()
          ! print *, "deleted11"
       end if
    end if

  end function surface_tracking

end module physics_routine
