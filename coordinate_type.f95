module coordinate_type
  use random
  implicit none

  type :: coordinate
     real :: x, y, z
  end type coordinate

contains

  subroutine show(this)
    type(coordinate), intent(in) :: this
    print *, this%x, this%y, this%z
  end subroutine show

  function distance(from, to)
    type(coordinate) :: from, to
    real :: distance
    distance=((to%x-from%x)**2+(to%y-from%y)**2+(to%z-from%z)**2)**0.5
  end function distance

  function generate_random(xmin, xmax, ymin, ymax, zmin, zmax)
    real :: xmin, xmax, ymin, ymax, zmin, zmax
    type(coordinate) :: generate_random
    generate_random%x=rng(xmin, xmax)
    generate_random%y=rng(ymin, ymax)
    generate_random%z=rng(zmin, zmax)
  end function generate_random

  function random_emission_direction()
    type(coordinate) :: random_emission_direction
    real :: azimuthal_angle, polar_angle

    azimuthal_angle=2*3.14159265*std_uniform_distribution()
    polar_angle=3.14159265*std_uniform_distribution()
    random_emission_direction%x=cos(azimuthal_angle)*sin(polar_angle)
    random_emission_direction%y=sin(azimuthal_angle)*sin(polar_angle)
    random_emission_direction%z=cos(polar_angle)
  end function random_emission_direction

end module coordinate_type
