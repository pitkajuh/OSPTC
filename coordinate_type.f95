module coordinate_type
  implicit none

  type :: coordinate
     real :: x, y, z
   contains

  end type coordinate

contains

  subroutine show(this)
    type(coordinate), intent(in) :: this
    print *, this%x, this%y, this%z
  end subroutine show

end module coordinate_type
