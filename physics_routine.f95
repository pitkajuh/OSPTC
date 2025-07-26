module physics_routine
  use random
  use tape_type
  use interpolate
  implicit none

contains

  function coherent_scattering_reaction(coherent_factor, n, energy, A, n2, n3) result(mu)
    real(kind(1.d0)), intent(in), allocatable :: coherent_factor(:, :)
    real(kind(1.d0)), intent(in), allocatable :: A(:, :)
    real(kind(1.d0)), intent(in) :: energy
    integer, intent(in) :: n, n2, n3
    real(kind(1.d0)) :: hc, ymax, deltay, Amax, y, Avalue, rand, energymin, ymin, mu
    integer :: i
    hc=4.135667696e-15_8*299792458.0_8
    ymax=(energy/hc)**2
    energymin=1E3_8
    ymin=(energymin/hc)**2
    deltay=(ymax-ymin)/n3
    rand=std_uniform_distribution()
    mu=0.0_8
    ! Amax=0.0_8

    ! Amax=Amax+0.5_8*deltay*(F1((n*deltay)**0.5_8, coherent_factor, n2)+&
    !      F1(A(1, 2)**0.5_8, coherent_factor, n2))

    ! do i=3, n
    !    Amax=Amax+deltay*F1(A(1, i)**0.5_8, coherent_factor, n2)
    !    ! write(new1, *) A(1, i), ";", A(2, i)
    !    ! if(i<5) print *, A(1, i), A(2, i)
    !    ! print *, i,n,A(1, i), A(2, i)
    ! end do

    Amax=linear_interpolation(A, ymax, n)
    ! Avalue=Amax

    do
       Avalue=rand*Amax

       do i=1, n3
          if(A(2, i)<=Avalue .and. Avalue<A(2, i+1)) exit
       end do

       y=A(1, i)+(Avalue-A(2, i))*((A(1, i+1)-A(1, i))/(A(2, i+1)-A(2, i)))
       mu=1-2*y/ymax

       if(rand<(1+mu)/2) exit
       rand=std_uniform_distribution()
    end do

    print *, energy,  mu
  end function coherent_scattering_reaction

  subroutine sum1(limits, records, energy, n, total, i)
    real(kind(1.d0)), intent(in) :: energy
    real(kind(1.d0)), intent(inout) :: total
    real(kind(1.d0)), intent(inout), dimension(:) :: limits
    integer, intent(in) :: n, i
    real(kind(1.d0)), allocatable :: records(:, :)
    real(kind(1.d0)) :: r
    r=linear_interpolation(records, energy, n)
    limits(i)=r+limits(i-1)
    total=total+r
  end subroutine sum1

  function select_reaction(endf, energy) result(reaction_id)
    type(tape), intent(inout) :: endf
    real(kind(1.d0)), intent(in) :: energy
    real(kind(1.d0)) :: r, total, random_value
    real(kind(1.d0)), dimension(4+endf%mf23%n_ionization+1) :: limits
    integer :: i, reaction_id

    r=0
    limits(1)=0.0
    total=0

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
    integer :: reaction_id, n1
    real(kind(1.d0)) :: Amax
    n1=500
    reaction_id=select_reaction(endf, energy)
    Amax=coherent_scattering_reaction(endf%mf27%coherent_factor%records, &
         endf%Ax, energy, endf%coherent_A, &
         endf%mf27%coherent_factor%n, endf%Ax)




    ! select case (reaction_id)
    ! case(1)
    !    print *, "coherent scattering"
    !    ! call coherent_scattering_reaction(endf%mf27%coherent_factor%records, &
    !    !      endf%mf27%coherent_factor%n, energy, 100.0_8, endf%coherent_A)
    ! case(2)
    !    print *, "incoherent scattering"
    ! case(3)
    !    print *, "pair formation in electric field"
    ! case(4)
    !    print *, "pair formation in nuclear field"
    ! case default
    !    print *, "ionization", reaction_id
    ! end select
  end subroutine reaction_function

end module physics_routine
