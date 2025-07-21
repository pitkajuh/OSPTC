module physics_routine
  use random
  use tape_type
  use interpolate
  implicit none

contains

  subroutine coherent_scattering_reaction(coherent_factor, n, energy, deltae, A)
    real(kind(1.d0)), allocatable :: coherent_factor(:, :)
    real(kind(1.d0)), allocatable :: A(:)
    real(kind(1.d0)), intent(in) :: energy, deltae
    integer, intent(in) :: n
    real(kind(1.d0)) :: hc, ymax, value1
    hc=4.135667696e-15_8*299792458.0_8
    ymax=energy/hc
    value1=0.0_8

    ! At high x, F approaches zero.
    ! if(ymax>1e9_8) then
    !    ! print *, "ymax>1e9"
    !    value1=39.6915375156568_8*ymax**(-2.9845939684527_8)
    !    value1=value1*value1
    ! else
       value1=F1(ymax, coherent_factor, n)
    ! end if
    ! print *, "ymax ", ymax, ymax**0.5, c
    print *, "value", value1, "ymax", ymax
    ! do
    !    if(x2>ymax) exit
    !    a1=0.5*deltax*(F1(x2, coherent_factor, n)+F1(x2-deltax, &
    !         coherent_factor, n))
    !    sum1=sum1+a1
    !    ! A(i)=A(i-1)+a1
    !    x2=x2+deltax
    !    ! i=i+1
    ! end do
    ! print *, "sum1", sum1
  end subroutine coherent_scattering_reaction

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
    reaction_id=select_reaction(endf, energy)
    call coherent_scattering_reaction(endf%mf27%coherent_factor%records, &
         endf%mf27%coherent_factor%n, energy, energy/1000.0_8, endf%coherent_A)




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
