module surface_type
  use coordinate_type
  implicit none

  type, abstract :: surface
     ! For general quadratic surface S(x, y, z)=Ax^2+By^2+Cz^2+Dxy+Eyz+F zx+Gx+Hy+Iz+J
     real(kind(1.d0)) :: value1=0, A=0, B=0, C=0, D=0, E=0, F=0, G=0, H=0, I=0, J=0, L=0, M=0, K=0
   contains
     procedure(create_interface), deferred :: create
     procedure(create_surface_equation), deferred :: surface_equation
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
  end interface

  type, extends(surface) :: planex
   contains
     procedure, pass :: create => create_planex
     procedure, pass :: surface_equation => surface_equation_x
  end type planex

  type, extends(surface) :: planey
   contains
     procedure, pass :: create => create_planey
     procedure, pass :: surface_equation => surface_equation_y
  end type planey

  type, extends(surface) :: planez
   contains
     procedure, pass :: create => create_planez
     procedure, pass :: surface_equation => surface_equation_z
  end type planez

contains

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

end module surface_type
