module physics_routine
  use random
  use tape_type
  use interpolate
  use photon_angular_distribution
  implicit none

contains

  function coherent_scattering_reaction(coherent_factor, n, energy, A, n2, n3) result(angle)
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    real(kind(1.d0)), intent(in), allocatable :: A(:, :)
    real(kind(1.d0)), intent(in) :: energy
    integer, intent(in) :: n, n2, n3
    real(kind(1.d0)) :: Amax, Avalue, rand, mu, angle, x2, x2max
    integer :: i
    x2max=x(energy, -1.0_8)**2
    rand=std_uniform_distribution()
    mu=0.0_8
    Amax=linear_interpolation(A, x2max, n, 1, 2)

    do
       Avalue=rand*Amax
       i=binary_search(A, Avalue, n3, 2)
       x2=A(1, i)+(Avalue-A(2, i))*((A(1, i+1)-A(1, i))/(A(2, i+1)-A(2, i)))
       mu=1-2*x2/x2max

       if(rand<(1+mu)/2) exit
       rand=std_uniform_distribution()
    end do

    angle=acos(mu)
    print *, energy,  mu, angle*(360/3.141592653589793)
  end function coherent_scattering_reaction

  function incoherent_scattering_reaction(energy) result(angle)
    real(kind(1.d0)), intent(in) :: energy
    ! real(kind(1.d0)) :: energy
    real(kind(1.d0)) :: epsilon0, electron_mass, alpha1, alpha2, &
         rand, epsilon, epsilonprime, rand2, rand3, rand4, rand5, k0prime, t, g, anglef, angle, c, energy1
    ! c=299792458.0_8
    ! energy1=energy*1.602176634E-19
    ! ! electron_mass=510998.9500001474_8
    ! electron_mass=9.1093837139E-31
    ! ! electron_mass=electron_mass*c*c
    ! epsilon=0.0_8
    ! epsilonprime=0.0_8
    ! k0prime=energy1/(electron_mass*c*c)
    ! epsilon0=1+2*energy1/(electron_mass*c*c)
    ! alpha1=log(1/epsilon0)
    ! alpha2=(1-epsilon0*epsilon0)/2
    ! ! print *, energy1
    ! rand=std_uniform_distribution()
    ! rand2=std_uniform_distribution()

    ! do
    !    epsilon=0.0_8

    !    if(alpha1>=(alpha1+alpha2)*rand) then
    !       epsilon=epsilon0*dexp(alpha1*rand2)
    !    else
    !       rand3=std_uniform_distribution()
    !       rand4=std_uniform_distribution()
    !       epsilonprime=rand3

    !       if(k0prime>=(k0prime+1)*rand2) then
    !          epsilonprime=max(rand3, rand4)
    !       end if

    !       epsilon=epsilon0+(1-epsilon0)*epsilonprime
    !    end if

    !    ! t=electron_mass*(1-epsilon)/(epsilon*energy1/electron_mass)
    !    t=electron_mass*c*c*(1-epsilon)/(energy1*epsilon)
    !    ! t=electron_mass*c*c*(1-epsilon)/(energy1*epsilon/(electron_mass*c*c))
    !    anglef=t*(2-t)
    !    g=1-((epsilon*anglef)/(1+epsilon*epsilon))
    !    rand5=std_uniform_distribution()
    !    print *, rand5, g, epsilon
    !    ! if(g<1.0_8) print *, rand5, g
    !    if(rand5>g .or. rand4>g) exit

    !    rand=std_uniform_distribution()
    !    rand2=std_uniform_distribution()
    !    ! exit
    ! end do

    ! anglef=0
    ! angle=asin(anglef**0.5)
    ! print *, "angle", angle
  end function incoherent_scattering_reaction

  subroutine sum1(limits, records, energy, n, total, i)
    real(kind(1.d0)), intent(in) :: energy
    real(kind(1.d0)), intent(inout) :: total
    real(kind(1.d0)), intent(inout), dimension(:) :: limits
    integer, intent(in) :: n, i
    real(kind(1.d0)), allocatable :: records(:, :)
    real(kind(1.d0)) :: r
    r=linear_interpolation(records, energy, n, 1, 2)
    limits(i)=r+limits(i-1)
    total=total+r
  end subroutine sum1

  function select_reaction(endf, energy) result(reaction_id)
    type(tape), intent(inout) :: endf
    real(kind(1.d0)), intent(in) :: energy
    real(kind(1.d0)) :: r, total, random_value
    real(kind(1.d0)), dimension(4+endf%mf23%n_ionization+1) :: limits
    integer :: i, reaction_id

    r=0.0_8
    limits(1)=0.0
    total=0.0_8

    call sum1(limits, endf%mf23%coherent_scattering%records, &
         energy, endf%mf23%coherent_scattering%n, total, 2)
    call sum1(limits, endf%mf23%incoherent_scattering%records, &
         energy, endf%mf23%incoherent_scattering%n, total, 3)
    call sum1(limits, endf%mf23%pair_formation_elec%records, &
         energy, endf%mf23%pair_formation_elec%n, total, 4)
    call sum1(limits, endf%mf23%pair_formation_nuc%records, &
         energy, endf%mf23%pair_formation_nuc%n, total, 5)

    do i=1, endf%mf23%n_ionization
       call sum1(limits, endf%mf23%photo_ionization(i)%records, &
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

    ! print *, i-1, limits(i-1)/total, random_value, limits(i)/total
    reaction_id=i-1
  end function select_reaction

  subroutine reaction_function(endf, energy)
    type(tape), intent(inout) :: endf
    real(kind(1.d0)), intent(in) :: energy
    integer :: reaction_id
    real(kind(1.d0)) :: angle


    ! angle=incoherent_scattering_reaction(energy)


    ! reaction_id=select_reaction(endf, energy)

    ! select case (reaction_id)
    ! case(1)
    !    print *, "coherent scattering"
       angle=coherent_scattering_reaction(endf%mf27%coherent_factor%records, &
            endf%Ax, energy, endf%coherent_A, &
            endf%mf27%coherent_factor%n, endf%Ax)
    ! case(2)
    !    print *, "incoherent scattering"
    ! case(3)
    !    print *, "pair formation in electric field"
    ! case(4)
    !    print *, "pair formation in nuclear field"
    ! case default
    !    print *, "ionization", reaction_id
    ! end select
       print *, "angle", angle*(360/3.141592653589793)
  end subroutine reaction_function

end module physics_routine
