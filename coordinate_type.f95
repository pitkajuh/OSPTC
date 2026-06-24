module coordinate_type
  use random
  implicit none

  type :: coordinate
     real(kind(1.d0)) :: x, y, z
  end type coordinate

  interface operator(+)
     module procedure add_coordinate
     module procedure add_scalar
     module procedure add_scalar_reverse
  end interface operator(+)

  interface operator(-)
     module procedure subtract_coordinate
     module procedure subtract_scalar
     module procedure subtract_scalar_reverse
  end interface operator(-)

  interface operator(*)
     module procedure multiply_coordinate
     module procedure multiply_scalar
     module procedure multiply_scalar_reverse
  end interface operator(*)

contains

  pure function add_coordinate(v1, v2)
    type(coordinate), intent(in) :: v1, v2
    type(coordinate) :: add_coordinate

    add_coordinate%x=v1%x+v2%x
    add_coordinate%y=v1%y+v2%y
    add_coordinate%z=v1%z+v2%z
  end function add_coordinate

  pure function add_scalar(v1, v2)
    type(coordinate), intent(in) :: v1
    real(kind(1.d0)), intent(in) :: v2
    type(coordinate) :: add_scalar

    add_scalar%x=v1%x+v2
    add_scalar%y=v1%y+v2
    add_scalar%z=v1%z+v2
  end function add_scalar

  pure function add_scalar_reverse(v1, v2)
    type(coordinate), intent(in) :: v2
    real(kind(1.d0)), intent(in) :: v1
    type(coordinate) :: add_scalar_reverse

    add_scalar_reverse%x=v1+v2%x
    add_scalar_reverse%y=v1+v2%y
    add_scalar_reverse%z=v1+v2%z
  end function add_scalar_reverse

  pure function subtract_coordinate(v1, v2)
    type(coordinate), intent(in) :: v1, v2
    type(coordinate) :: subtract_coordinate

    subtract_coordinate%x=v1%x-v2%x
    subtract_coordinate%y=v1%y-v2%y
    subtract_coordinate%z=v1%z-v2%z
  end function subtract_coordinate

  pure function subtract_scalar(v1, v2)
    type(coordinate), intent(in) :: v1
    real(kind(1.d0)), intent(in) :: v2
    type(coordinate) :: subtract_scalar

    subtract_scalar%x=v1%x-v2
    subtract_scalar%y=v1%y-v2
    subtract_scalar%z=v1%z-v2
  end function subtract_scalar

  pure function subtract_scalar_reverse(v1, v2)
    type(coordinate), intent(in) :: v2
    real(kind(1.d0)), intent(in) :: v1
    type(coordinate) :: subtract_scalar_reverse

    subtract_scalar_reverse%x=v1-v2%x
    subtract_scalar_reverse%y=v1-v2%y
    subtract_scalar_reverse%z=v1-v2%z
  end function subtract_scalar_reverse

  pure function multiply_coordinate(v1, v2)
    type(coordinate), intent(in) :: v1, v2
    type(coordinate) :: multiply_coordinate

    multiply_coordinate%x=v1%x*v2%x
    multiply_coordinate%y=v1%y*v2%y
    multiply_coordinate%z=v1%z*v2%z
  end function multiply_coordinate

  pure function multiply_scalar(v1, v2)
    type(coordinate), intent(in) :: v1
    real(kind(1.d0)), intent(in) :: v2
    type(coordinate) :: multiply_scalar

    multiply_scalar%x=v1%x*v2
    multiply_scalar%y=v1%y*v2
    multiply_scalar%z=v1%z*v2
  end function multiply_scalar

  pure function multiply_scalar_reverse(v1, v2)
    type(coordinate), intent(in) :: v2
    real(kind(1.d0)), intent(in) :: v1
    type(coordinate) :: multiply_scalar_reverse

    multiply_scalar_reverse%x=v1*v2%x
    multiply_scalar_reverse%y=v1*v2%y
    multiply_scalar_reverse%z=v1*v2%z
  end function multiply_scalar_reverse

  subroutine show(this)
    type(coordinate), intent(in) :: this
    print *, this%x, this%y, this%z
  end subroutine show

  function distance(from, to)
    type(coordinate), intent(in) :: from, to
    real(kind(1.d0)) :: distance
    distance=((to%x-from%x)**2+(to%y-from%y)**2+(to%z-from%z)**2)**0.5
  end function distance

  function generate_random(xmin, xmax, ymin, ymax, zmin, zmax)
    real(kind(1.d0)), intent(in) :: xmin, xmax, ymin, ymax, zmin, zmax
    type(coordinate) :: generate_random
    generate_random%x=rng(xmin, xmax)
    generate_random%y=rng(ymin, ymax)
    generate_random%z=rng(zmin, zmax)
  end function generate_random

  ! function random_emission_direction()
  !   ! Create an unit vector pointing in random direction using spherical coordinates. The radius of the sphere is not sampled because the sampling is done from the origin of the sphere.
  !   type(coordinate) :: random_emission_direction
  !   real(kind(1.d0)) :: azimuthal_angle, polar_angle

  !   azimuthal_angle=2*3.14159265*std_uniform_distribution()
  !   polar_angle=3.14159265*std_uniform_distribution()
  !   random_emission_direction%x=sin(polar_angle)*cos(azimuthal_angle)
  !   random_emission_direction%y=sin(polar_angle)*sin(azimuthal_angle)
  !   random_emission_direction%z=cos(polar_angle)
  ! end function random_emission_direction

end module coordinate_type
