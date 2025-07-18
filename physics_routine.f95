module physics_routine
  use random
  use tape_type
  use interpolate
  implicit none

contains

  subroutine sum1(limits, records, energy, n, total, i)
    real(kind(1.d0)), intent(in) :: energy
    real(kind(1.d0)), intent(out) :: total
    real(kind(1.d0)), dimension(:) :: limits
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
    print *, energy, 4+endf%mf23%n_ionization+1, endf%mf23%n_ionization
    do i=2, 4+endf%mf23%n_ionization+1
       print *, i-1, limits(i)/total
    end do

    do i=2, 4+endf%mf23%n_ionization
       if(random_value<limits(i)/total) exit
    end do

    print *, i-1, limits(i-1)/total, random_value, limits(i)/total
    reaction_id=i-1
  end function select_reaction

  subroutine reaction_function(endf, energy)
    type(tape), intent(inout) :: endf
    real(kind(1.d0)), intent(in) :: energy
    integer :: reaction_id
    reaction_id=select_reaction(endf, energy)

    select case (reaction_id)
    case(1)
       print *, "coherent scattering"
    case(2)
       print *, "incoherent scattering"
    case(3)
       print *, "pair formation in electric field"
    case(4)
       print *, "pair formation in nuclear field"
    case default
       print *, "ionization", reaction_id
    end select
  end subroutine reaction_function

end module physics_routine
