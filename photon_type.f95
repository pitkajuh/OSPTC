module photon_type
  ! use random
  use constants, only: pi, pi2
  use coordinate_type
  implicit none

  type :: photon
     real(kind(1.d0)) :: energy
     type(coordinate) :: direction
     type(coordinate) :: origin
     type(coordinate) :: to
     type(coordinate) :: mfp

     ! real(kind(1.d0)), dimension(6, 4) :: header
     ! real(kind(1.d0)), allocatable :: coherent_A(:, :)
     ! real(kind(1.d0)), allocatable :: incoherent_A(:, :)
     ! integer, allocatable :: sizes(:)
     ! integer :: n, Ax, Ax1
     ! type(MF23) :: mf23
     ! type(MF27) :: mf27
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

  subroutine create_photon(this, energy)
    real(kind(1.d0)), intent(in) :: energy
    type(photon) :: this
    this%energy=energy
    call random_emission_direction(this)
  end subroutine create_photon
!   subroutine read_tape(tape_name)
!     ! type(tape) :: this
!     character(*) :: tape_name
!     integer :: ios, z, i
!     real(kind(1.d0)) :: emax, emin
!     z=1
!   end subroutine read_tape



end module photon_type
