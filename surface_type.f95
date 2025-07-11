module surface_type
  use coordinate_type
  implicit none

  type, abstract :: surface
     ! For general quadratic surface S(x, y, z)=Ax^2+By^2+Cz^2+Dxy+Eyz+F zx+Gx+Hy+Iz+J
     real(kind(1.d0)) :: value1=0, A=0, B=0, C=0, D=0, E=0, F=0, G=0, H=0, I=0, J=0, L=0, M=0, K=0
   contains
     procedure(create_interface), deferred :: create
     procedure(create_surface_equation), deferred :: surface_equation
     procedure(create_surface_distance), deferred :: surface_distance
     procedure(create_surface_test), deferred :: surface_test
  end type surface

  abstract interface
     subroutine create_interface(this, v)
       import surface
       class(surface), intent(inout) :: this
       real, intent(in) :: v
     end subroutine create_interface

     function create_surface_equation(this, p) result(result1)
       use coordinate_type
       import surface
       class(surface), intent(inout) :: this
       type(coordinate), intent(in) :: p
       real :: result1
     end function create_surface_equation

     function create_surface_distance(this, from, to) result(result1)
       use coordinate_type
       import surface
       class(surface), intent(inout) :: this
       type(coordinate), intent(in) :: from, to
       real :: result1
     end function create_surface_distance

     function create_surface_test(this, p) result(result1)
       use coordinate_type
       import surface
       class(surface), intent(inout) :: this
       type(coordinate), intent(in) :: p
       logical :: result1
     end function create_surface_test
  end interface

  type, extends(surface) :: planex
   contains
     procedure, pass :: create => create_planex
     procedure, pass :: surface_equation => surface_equation_x
     procedure, pass :: surface_distance => surface_distance_x
     procedure, pass :: surface_test => surface_test_x
  end type planex

  type, extends(surface) :: planey
   contains
     procedure, pass :: create => create_planey
     procedure, pass :: surface_equation => surface_equation_y
     procedure, pass :: surface_distance => surface_distance_y
     procedure, pass :: surface_test => surface_test_y
  end type planey

  type, extends(surface) :: planez
   contains
     procedure, pass :: create => create_planez
     procedure, pass :: surface_equation => surface_equation_z
     procedure, pass :: surface_distance => surface_distance_z
     procedure, pass :: surface_test => surface_test_z
  end type planez

contains

  function get_surface_test(v)
    real :: v
    logical :: get_surface_test

    ! Return true if the point is inside(<0) or on(==0) the surface.
    if(v<=0) then
       get_surface_test=.true.
    else
       get_surface_test=.false.
    end if
  end function get_surface_test

  subroutine create_planex(this, v)
    class(planex), intent(inout) :: this
    real, intent(in) :: v
    this%value1=v
    this%G=1
    this%J=-v
  end subroutine create_planex

  function surface_equation_x(this, p) result(result1)
    class(planex), intent(inout) :: this
    type(coordinate), intent(in) :: p
    real :: result1
    result1=p%x+this%J
  end function surface_equation_x

  function surface_distance_x(this, from, to) result(result1)
    class(planex), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    real :: result1

    if(to%x==0) then
       result1=-1
    else
       result1=-(from%x+this%J)/to%x
    end if
  end function surface_distance_x

  function surface_test_x(this, p) result(result1)
    class(planex), intent(inout) :: this
    type(coordinate), intent(in) :: p
    logical :: result1
    result1=get_surface_test(surface_equation_x(this, p))
  end function surface_test_x

  subroutine create_planey(this, v)
    class(planey), intent(inout) :: this
    real, intent(in) :: v
    this%value1=v
    this%H=1
    this%J=-v
  end subroutine create_planey

  function surface_equation_y(this, p) result(result1)
    class(planey), intent(inout) :: this
    type(coordinate), intent(in) :: p
    real :: result1
    result1=p%y+this%J
  end function surface_equation_y

  function surface_distance_y(this, from, to) result(result1)
    class(planey), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    real :: result1

    if(to%y==0) then
       result1=-1
    else
       result1=-(from%y+this%J)/to%y
    end if
  end function surface_distance_y

  function surface_test_y(this, p) result(result1)
    class(planey), intent(inout) :: this
    type(coordinate), intent(in) :: p
    logical :: result1
    result1=get_surface_test(surface_equation_y(this, p))
  end function surface_test_y

  subroutine create_planez(this, v)
    class(planez), intent(inout) :: this
    real, intent(in) :: v
    this%value1=v
    this%I=1
    this%J=-v
  end subroutine create_planez

  function surface_equation_z(this, p) result(result1)
    class(planez), intent(inout) :: this
    type(coordinate), intent(in) :: p
    real :: result1
    result1=p%z+this%J
  end function surface_equation_z

  function surface_distance_z(this, from, to) result(result1)
    class(planez), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    real :: result1

    if(to%z==0) then
       result1=-1
    else
       result1=-(from%z+this%J)/to%z
    end if
  end function surface_distance_z

  function surface_test_z(this, p) result(result1)
    class(planez), intent(inout) :: this
    type(coordinate), intent(in) :: p
    logical :: result1
    result1=get_surface_test(surface_equation_z(this, p))
  end function surface_test_z

end module surface_type
