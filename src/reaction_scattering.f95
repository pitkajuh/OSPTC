module reaction_scattering
  use random
  use tape_type
  use photon_type
  use constants, only: electron_mass
  implicit none

contains

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

end module reaction_scattering
