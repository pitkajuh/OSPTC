module surface_type
  use coordinate_type
  implicit none

  type, abstract :: surface
   contains
     procedure(create_surface_equation), deferred :: surface_equation
     procedure(create_surface_distance), deferred :: surface_distance
     procedure(create_surface_test), deferred :: surface_test
  end type surface

  abstract interface
     function create_surface_equation(this, p) result(result1)
       use coordinate_type
       import surface
       class(surface), intent(inout) :: this
       type(coordinate), intent(in) :: p
       real(kind(1.d0)) :: result1
     end function create_surface_equation

     function create_surface_distance(this, from, to) result(result1)
       use coordinate_type
       import surface
       class(surface), intent(inout) :: this
       type(coordinate), intent(in) :: from, to
       real(kind(1.d0)) :: result1
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
     real(kind(1.d0)) :: v, G, J
   contains
     procedure, pass :: surface_equation => surface_equation_x
     procedure, pass :: surface_distance => surface_distance_x
     procedure, pass :: surface_test => surface_test_x
  end type planex

  type, extends(surface) :: planey
     real(kind(1.d0)) :: v, H, J
   contains
     procedure, pass :: surface_equation => surface_equation_y
     procedure, pass :: surface_distance => surface_distance_y
     procedure, pass :: surface_test => surface_test_y
  end type planey

  type, extends(surface) :: planez
     real(kind(1.d0)) :: v, I, J
   contains
     procedure, pass :: surface_equation => surface_equation_z
     procedure, pass :: surface_distance => surface_distance_z
     procedure, pass :: surface_test => surface_test_z
  end type planez

  type, extends(surface) :: cylinder
     real(kind(1.d0)) :: v, J
     type(coordinate) :: centered_at
   contains
     procedure, pass :: surface_equation => surface_equation_cylinder
     procedure, pass :: surface_distance => surface_distance_cylinder
     procedure, pass :: surface_test => surface_test_cylinder
  end type cylinder

contains

  function get_surface_test(v)
    real(kind(1.d0)) :: v
    logical :: get_surface_test
    get_surface_test=.false.
    ! Return true if the point is inside(<0) or on(==0) the surface.
    if(v<=0) get_surface_test=.true.
  end function get_surface_test

  subroutine create_planex(this, v)
    class(planex), intent(inout) :: this
    real(kind(1.d0)), intent(in) :: v
    this%v=v
    this%G=1
    this%J=-v
  end subroutine create_planex

  function surface_equation_x(this, p) result(result1)
    class(planex), intent(inout) :: this
    type(coordinate), intent(in) :: p
    real(kind(1.d0)) :: result1
    result1=p%x+this%J
  end function surface_equation_x

  function surface_distance_x(this, from, to) result(result1)
    class(planex), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    real(kind(1.d0)) :: result1
    result1=-1
    if(to%x>0) result1=-(from%x+this%J)/to%x
  end function surface_distance_x

  function surface_test_x(this, p) result(result1)
    class(planex), intent(inout) :: this
    type(coordinate), intent(in) :: p
    logical :: result1
    result1=get_surface_test(surface_equation_x(this, p))
  end function surface_test_x

  subroutine create_planey(this, v)
    class(planey), intent(inout) :: this
    real(kind(1.d0)), intent(in) :: v
    this%v=v
    this%H=1
    this%J=-v
  end subroutine create_planey

  function surface_equation_y(this, p) result(result1)
    class(planey), intent(inout) :: this
    type(coordinate), intent(in) :: p
    real(kind(1.d0)) :: result1
    result1=p%y+this%J
  end function surface_equation_y

  function surface_distance_y(this, from, to) result(result1)
    class(planey), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    real(kind(1.d0)) :: result1
    result1=-1
    if(to%y>0) result1=-(from%y+this%J)/to%y
  end function surface_distance_y

  function surface_test_y(this, p) result(result1)
    class(planey), intent(inout) :: this
    type(coordinate), intent(in) :: p
    logical :: result1
    result1=get_surface_test(surface_equation_y(this, p))
  end function surface_test_y

  subroutine create_planez(this, v)
    class(planez), intent(inout) :: this
    real(kind(1.d0)), intent(in) :: v
    this%v=v
    this%I=1
    this%J=-v
  end subroutine create_planez

  function surface_equation_z(this, p) result(result1)
    class(planez), intent(inout) :: this
    type(coordinate), intent(in) :: p
    real(kind(1.d0)) :: result1
    result1=p%z+this%J
  end function surface_equation_z

  function surface_distance_z(this, from, to) result(result1)
    class(planez), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    real(kind(1.d0)) :: result1
    result1=-1
    if(to%z>0) result1=-(from%z+this%J)/to%z
  end function surface_distance_z

  function surface_test_z(this, p) result(result1)
    class(planez), intent(inout) :: this
    type(coordinate), intent(in) :: p
    logical :: result1
    result1=get_surface_test(surface_equation_z(this, p))
  end function surface_test_z

  subroutine create_cylinder(this, v, centered_at)
    class(cylinder), intent(out) :: this
    type(coordinate), intent(in) :: centered_at
    real(kind(1.d0)), intent(in) :: v
    this%v=v
    this%centered_at=centered_at
    this%J=-v*v
  end subroutine create_cylinder

  function surface_equation_cylinder(this, p) result(result1)
    class(cylinder), intent(inout) :: this
    type(coordinate), intent(in) :: p
    real(kind(1.d0)) :: result1
    result1=(p%x-this%centered_at%x)**2+(p%y-this%centered_at%y)**2+this%J
  end function surface_equation_cylinder

  function surface_distance_cylinder(this, from, to) result(result1)
    class(cylinder), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    type(coordinate) :: centered_at
    real(kind(1.d0)) :: result1, in_sqrt, positive, negative, K, L, M

    ! Equation should be A*(x-x0)*(x-x0)+B*(y-y0)*(y-y0)+J, but A and B are omitted because they are 1.

    K=(from%x-centered_at%x)**2+(from%y-centered_at%y)**2+this%J
    L=2*(to%x*(from%x-centered_at%x)+to%y*(from%y-centered_at%y))
    M=to%x*to%x+to%y*to%y

    in_sqrt=L*L-4*M*K

    if(in_sqrt<0 .or. M==0) then
       result1=-1
    else
       positive=(-L+in_sqrt**0.5)/(2*M)
       negative=(-L-in_sqrt**0.5)/(2*M)
       result1=negative

       if(positive<0 .and. negative<0) then
          ! No solution exists, the surface is away from line-of-sight.
          result1=-1
       else if(positive>0 .and. negative<0 .and. positive>negative) then
          result1=positive
       end if
    end if
  end function surface_distance_cylinder

  function surface_test_cylinder(this, p) result(result1)
    class(cylinder), intent(inout) :: this
    type(coordinate), intent(in) :: p
    logical :: result1
    result1=get_surface_test(surface_equation_cylinder(this, p))
  end function surface_test_cylinder

end module surface_type
