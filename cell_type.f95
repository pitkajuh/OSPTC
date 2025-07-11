module cell_type
  use surface_type
  implicit none

  type, abstract :: cell
     character :: name
     integer :: hits
   contains
     procedure(create_cell_test), deferred :: cell_test
  end type cell

  abstract interface
     function create_cell_test(this, p) result(result1)
       use coordinate_type
       import cell
       class(cell), intent(inout) :: this
       type(coordinate), intent(in) :: p
       logical :: result1
     end function create_cell_test
  end interface

  type, extends(cell) :: cell_box_3d
     type(planex) :: wallx_negative
     type(planex) :: wallx_positive
     type(planey) :: wally_negative
     type(planey) :: wally_positive
     type(planez) :: wallz_negative
     type(planez) :: wallz_positive
   contains
     procedure, pass :: cell_test => cell_test_box
  end type cell_box_3d

contains

  function cell_test_box(this, p) result(result1)
    class(cell_box_3d), intent(inout) :: this
    type(coordinate), intent(in) :: p
    logical :: result1
  end function cell_test_box

end module cell_type
