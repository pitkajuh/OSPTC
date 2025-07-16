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
     subroutine create_interface(this)
       import :: surface
       class(surface), intent(in) :: this
     end subroutine create_interface

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
     procedure :: create => create_planex
     procedure, pass :: surface_equation => surface_equation_x
     procedure, pass :: surface_distance => surface_distance_x
     procedure, pass :: surface_test => surface_test_x
  end type planex

  type, extends(surface) :: planey
     real(kind(1.d0)) :: v, H, J
   contains
     procedure :: create => create_planey
     procedure, pass :: surface_equation => surface_equation_y
     procedure, pass :: surface_distance => surface_distance_y
     procedure, pass :: surface_test => surface_test_y
  end type planey

  type, extends(surface) :: planez
     real(kind(1.d0)) :: v, I, J
   contains
     procedure :: create => create_planez
     procedure, pass :: surface_equation => surface_equation_z
     procedure, pass :: surface_distance => surface_distance_z
     procedure, pass :: surface_test => surface_test_z
  end type planez

  type, extends(surface) :: cylinder
     real(kind(1.d0)) :: v, J
     type(coordinate) :: centered_at
   contains
     ! procedure, pass :: create => create_cylinder
     procedure :: create => create_cylinder
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
    result1=-(from%x+this%J)/to%x
    if(to%x==0) result1=-1
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
    result1=-(from%y+this%J)/to%y
    if(to%y==0) result1=-1
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
    result1=-(from%z+this%J)/to%z
    if(to%z==0) result1=-1
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
    result1=(p%x-this%centered_at%x)**2+(p%y-this%centered_at%y)**2+this%J;
  end function surface_equation_cylinder

  function surface_distance_cylinder(this, from, to) result(result1)
    class(cylinder), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    type(coordinate) :: centered_at
    real(kind(1.d0)) :: result1, in_sqrt
    real(kind(1.d0)) :: option_positive
    real(kind(1.d0)) :: option_negative
    real(kind(1.d0)) :: x, y, x0, y0, u, v, K, L, M

    ! Equation should be A*(x-x0)*(x-x0)+B*(y-y0)*(y-y0)+J, but A and B are omitted because they are 1.

    x=from%x
    y=from%y
    x0=centered_at%x
    y0=centered_at%y
    ! Direction vector by using direction cosine.
    u=to%x
    v=to%y

    K=(x-x0)**2+(y-y0)**2+this%J;
    L=2*(u*(x-x0)+v*(y-y0));
    M=u*u+v*v;

    in_sqrt=L*L-4*M*K;

    if(in_sqrt<0 .or. M==0) then
       result1=-1;
    else
       option_positive=(-L+in_sqrt**0.5)/(2*M)
       option_negative=(-L-in_sqrt**0.5)/(2*M)
       result1=option_negative

       if(option_positive<0 .and. option_negative<0) then
          ! No solution exists, the surface is away from line-of-sight.
          result1=-1
       else if(option_positive>0 .and. option_negative<0 .and. option_positive>option_negative) then
          result1=option_positive
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

module surface_creator
  use surface_type, only: surface, cylinder
  use coordinate_type
  implicit none

  interface create
     module procedure create_cylinder1
  end interface create

contains

  function create_cylinder1(value1, value2, centered_at) result(cyl)
    type(coordinate), intent(in) :: centered_at
    real(kind(1.d0)), intent(in) :: value1, value2
    class(surface), allocatable :: cyl
    allocate(cylinder :: cyl)
    cyl=cylinder(value1, value2, centered_at)
  end function create_cylinder1
end module surface_creator
