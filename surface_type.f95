module surface_type
  implicit none

  type, abstract :: surface
     ! For general quadratic surface S(x, y, z)=Ax^2+By^2+Cz^2+Dxy+Eyz+F zx+Gx+Hy+Iz+J
     real(kind(1.d0)) :: value1=0, A=0, B=0, C=0, D=0, E=0, F=0, G=0, H=0, I=0, J=0, L=0, M=0, K=0
   contains
     procedure(create1), deferred :: create
  end type surface

  abstract interface
     subroutine create1(this, v)
       import surface
       class(surface), intent(inout) :: this
       real(kind(1.d0)), intent(in) :: v
     end subroutine create1
  end interface

  ! type, extends(surface) :: planex
  !  contains
  !    procedure :: create => create_planex
  ! end type planex

! contains

!   subroutine create_planex(this, v)
!     class(planex), intent(inout) :: this
!     real(kind(1.d0)), intent(in) :: v
!     this%value1=v
!     this%G=1
!     this%J=-v
!   end subroutine create_planex

end module surface_type

! module planex_type
!   use surface_type
!   implicit none

!   type, extends(surface) :: planex

!   end type planex
! contains
!   subroutine create_plane(v)
!     implicit none
!     real(kind(1.d0)) :: v
!     ! %value1=v
!     ! %G=1
!     ! %J=-v
!   end subroutine create_plane
! end module planex_type
