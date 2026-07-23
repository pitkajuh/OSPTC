module photon_type
  use constants, only: pi, pi2
  use coordinate_type
  implicit none

  type :: photon
     integer :: id
     real(kind(1.d0)) :: energy
     type(coordinate) :: direction
     type(coordinate) :: origin
     ! type(coordinate) :: to
     ! type(coordinate) :: mfp
     type(photon), pointer :: next_photon => null()
     type(photon), pointer :: previous_photon => null()
  end type photon

contains

  subroutine random_emission_direction(this)
    ! Create an unit vector pointing in random direction
    ! using spherical coordinates. The radius of the sphere
    ! is not sampled because the sampling is done from the
    ! origin of the sphere.
    type(photon) :: this
    type(coordinate) :: random_emission_direction1
    real(kind(1.d0)) :: azimuthal_angle, polar_angle

    azimuthal_angle=pi2*std_uniform_distribution()
    polar_angle=pi*std_uniform_distribution()
    random_emission_direction1%x=sin(polar_angle)*cos(azimuthal_angle)
    random_emission_direction1%y=sin(polar_angle)*sin(azimuthal_angle)
    random_emission_direction1%z=cos(polar_angle)
    this%direction=random_emission_direction1
  end subroutine random_emission_direction

  function calculate_mfp(this, mu, density) result(mfp)
    type(photon), intent(inout) :: this
    real(kind(1.d0)), intent(in) :: mu, density
    type(coordinate) :: mfp
    ! this%to=(-1/mu)*this%direction*log(rng(0.0_8, 1.0_8))
    mfp=this%origin+0.01_8*(-1.0_8/(mu*density))*this%direction*log(rng(0.0_8, 1.0_8))
  end function calculate_mfp

  subroutine create_photon(this, energy, origin)
    real(kind(1.d0)), intent(in) :: energy
    type(coordinate), intent(in) :: origin
    type(photon) :: this
    this%origin=origin
    this%energy=energy
    call random_emission_direction(this)
  end subroutine create_photon

end module photon_type
