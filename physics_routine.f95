module physics_routine
  use random
  use tape_type
  use interpolate
  use cell_type
  use photon_type
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
    print *, "pair"

    ! Create photon going left
    call create_annihilation_photon(left, multiply_scalar(mfp, -1.0_8), mfp, ph%id+1)
    ! Connect to head photon
    left%previous_photon=>ph
    ph%next_photon=>left

    ! Create photon going right
    call create_annihilation_photon(right, mfp, mfp, ph%id+2)
    right%previous_photon=>left
    left%next_photon=>right


    temporary_photon=>ph
    ph=>ph%next_photon
    deallocate(temporary_photon)



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

    current_photon=>ph

    do while(associated(current_photon))
       print *, current_photon%energy, current_photon%id
       ! if(associated(current_photon%next_photon)) then
       !    current_photon%next_photon=>left
       !    exit
       ! end if
       current_photon=>current_photon%next_photon
    end do


    ! ph%next_photon
  end subroutine pair_production

  function photo_ionization(ph, endf, reaction_id) result(end)
    type(tape), intent(in) :: endf
    type(photon), pointer, intent(inout) :: ph
    integer, intent(in) :: reaction_id
    real(kind(1.d0)) :: ionization_energy
    logical :: end
    end=.true.
    ionization_energy=endf%mf23%photo_ionization(reaction_id-4)%header(2, 1)

    if(ionization_energy<ph%energy) then
       ph%energy=ph%energy-ionization_energy
       end=.false.
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
    real(kind(1.d0)) :: Amax, Avalue, rand, mu, x_value, xmax, a1
    integer :: i
    xmax=x(energy, -1.0_8)
    mu=0.0_8
    ! print *, "sample"
    ! print *, xmax
    Amax=linear_interpolation(A, xmax, n, 1, 2)
    a1=energy/electron_mass
    i=0
    do
       mu=kahns_method(a1)
       rand=std_uniform_distribution()
       x_value=x(energy, mu)
       Avalue=linear_interpolation(A, x_value, n, 1, 2)
       i=i+1
       ! print *, i, Avalue/Amax
       if(rand<=Avalue/Amax) exit
    end do
    ! print *, i, mu
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

  function select_reaction(endf, energy) result(reaction_id)
    type(tape), intent(inout) :: endf
    real(kind(1.d0)), intent(in) :: energy
    real(kind(1.d0)) :: r, total, random_value
    real(kind(1.d0)), dimension(4+endf%mf23%n_ionization+1) :: limits
    integer :: i, reaction_id

    i=2
    r=0.0_8
    limits(1)=0.0
    total=0.0_8

    call sum_cross_sections(limits, endf%mf23%coherent_scattering%records, &
         energy, endf%mf23%coherent_scattering%n, total, 2)
    call sum_cross_sections(limits, endf%mf23%incoherent_scattering%records, &
         energy, endf%mf23%incoherent_scattering%n, total, 3)
    call sum_cross_sections(limits, endf%mf23%pair_formation_elec%records, &
         energy, endf%mf23%pair_formation_elec%n, total, 4)
    call sum_cross_sections(limits, endf%mf23%pair_formation_nuc%records, &
         energy, endf%mf23%pair_formation_nuc%n, total, 5)

    do i=1, endf%mf23%n_ionization
       ! print *, i, endf%mf23%n_ionization, total, limits(i)
       call sum_cross_sections(limits, endf%mf23%photo_ionization(i)%records, &
            energy, endf%mf23%photo_ionization(i)%n, total, 5+i)
    end do

    random_value=std_uniform_distribution()
    ! print *, energy, 4+endf%mf23%n_ionization+1, endf%mf23%n_ionization
    ! do i=2, 4+endf%mf23%n_ionization+1
    !    print *, i-1, limits(i)/total
    ! end do

    ! do i=1, 4+endf%mf23%n_ionization
    !    print *, i, limits(i)/total
    ! end do

    do i=2, 4+endf%mf23%n_ionization
       if(random_value<limits(i)/total) then
          exit
       end if
    end do
    ! deallocate(limits)
    ! print *, i-1, limits(i-1)/total, random_value, limits(i)/total
    reaction_id=i-1
  end function select_reaction

  function reaction_function(endf, ph, mfp) result(end)
    type(tape), intent(inout) :: endf
    type(photon), pointer, intent(inout) :: ph
    type(coordinate), intent(in) :: mfp
    integer :: reaction_id
    real(kind(1.d0)) :: angle
    logical :: end
    end=.false.
    reaction_id=select_reaction(endf, ph%energy)

    select case (reaction_id)
    case(1)
       print *, "coherent scattering", ph%energy
       call coherent_scattering_reaction(ph, endf)
    case(2)
       print *, "incoherent scattering", ph%energy
       call incoherent_scattering_reaction(ph, endf)
    case(3)
       print *, "pair formation in electric field", ph%energy
       call pair_production(ph, mfp)
    case(4)
       print *, "pair formation in nuclear field", ph%energy
       call pair_production(ph, mfp)
    case default
       print *, "ionization", ph%energy
       end=photo_ionization(ph, endf, reaction_id)
    end select

  end function reaction_function

  function surface_tracking(cell_all, ph, cell_from) result(end)
    class(cells), intent(inout), allocatable :: cell_all(:)
    type(photon), pointer, intent(inout) :: ph
    integer, intent(inout) :: cell_from
    type(photon), pointer :: current_photon, temp
    integer :: cell_index, k, j!, cell_index_new
    logical :: end, has_next, has_previous
    real(kind(1.d0)) :: distance_to_cell
    type(coordinate) :: mfp
    end=.false.
    ! cell_index_new=cell_from
    ! cell_from=1
    ! print *, "from", cell_from
    do k=1, 1000
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
             ! print *, distance_to_cell, ph%origin, ph%direction

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
          ! temp=>ph

          ! if(.not. has_previous) then
          !    ! Current photon is head.
          !    ph=>ph%next_photon
          ! else if(.not. has_next) then
          !    ! Current photon is last.
          !    ph=>ph%previous_photon
          ! else
          !    ! Current photon is in the middle.
          !    ph%previous_photon=>ph%next_photon
          !   ! ph%next_photon=>
          ! end if
          ! deallocate(temp)

          print *, "exited", cell_from, cell_index
          call show(mfp)
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
       print *, ph%energy
    end do

    if(end .eqv. .true.) then
       print *, "id", ph%id
       ! current_photon=>ph

       ! do while(associated(current_photon))
       !    if()
       ! end do
    end if


  end function surface_tracking

end module physics_routine
