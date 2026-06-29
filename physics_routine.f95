module physics_routine
  use random
  use tape_type
  use interpolate
  use photon_type
  use photon_angular_distribution
  use constants, only: electron_mass
  implicit none

contains

  function sample_coherent_scattering_angle(n, energy, A) result(mu)
    real(kind(1.d0)), intent(in), allocatable :: A(:, :)
    real(kind(1.d0)), intent(in) :: energy
    integer, intent(in) :: n
    real(kind(1.d0)) :: Amax, Avalue, rand, mu, x2, x2max
    integer :: i!, j
    x2max=x(energy, -1.0_8)**2
    mu=0.0_8
    Amax=linear_interpolation(A, x2max, n, 1, 2)
    i=0
    ! j=0

    do
       rand=std_uniform_distribution()
       Avalue=rand*Amax
       i=binary_search(A, Avalue, n, 2)
       x2=A(1, i)+(Avalue-A(2, i))*((A(1, i+1)-A(1, i))/(A(2, i+1)-A(2, i)))
       mu=1-2*x2*x2max

    !   j=j+1
       if(rand<0.5*(1+mu)) exit
    end do
    ! print *, j, mu
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
    type(photon), intent(inout) :: ph
    real(kind(1.d0)) :: mu
    mu=sample_incoherent_scattering_angle(endf%n_incoherent, ph%energy, endf%incoherent_A)
    ph%energy=ph%energy/(1+(ph%energy/electron_mass)*(1-mu))
    ph%direction=create_unit_vector(ph%direction*mu)
    ! print *, length(ph%direction)
  end subroutine incoherent_scattering_reaction

  subroutine coherent_scattering_reaction(ph, endf)
    type(tape), intent(inout) :: endf
    type(photon), intent(inout) :: ph
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
    ! allocate(limits(4+endf%mf23%n_ionization+1))
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

    do i=2, 4+endf%mf23%n_ionization
       if(random_value<limits(i)/total) exit
    end do
    ! deallocate(limits)
    ! print *, i-1, limits(i-1)/total, random_value, limits(i)/total
    reaction_id=i-1
  end function select_reaction

  subroutine reaction_function(endf, ph)
    type(tape), intent(inout) :: endf
    type(photon), intent(inout) :: ph
    integer :: reaction_id
    real(kind(1.d0)) :: angle
    reaction_id=select_reaction(endf, ph%energy)

    select case(reaction_id)
    case(1)
       call coherent_scattering_reaction(ph, endf)
    case default
       call incoherent_scattering_reaction(ph, endf)
    end select

    ! select case (reaction_id)
    ! case(1)
    !    ! print *, "coherent scattering"
    !    call coherent_scattering_reaction(ph, endf)
    ! case(2)
    !    ! print *, "incoherent scattering"
    !    call incoherent_scattering_reaction(ph, endf)
    ! case(3)
    !    ! print *, "pair formation in electric field"
    ! case(4)
    !    ! print *, "pair formation in nuclear field"
    ! case default
    !    ! print *, "ionization", reaction_id
    ! end select
    !    ! print *, "angle", angle*(360/3.141592653589793)
  end subroutine reaction_function

end module physics_routine
