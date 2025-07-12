module cell_type
  use surface_type
  implicit none

  type, abstract :: cell
     character :: name
     integer :: hits
     ! type(material) :: material
   contains
     procedure(create_cell_test), deferred :: cell_test
     procedure(create_cell_distance), deferred :: cell_distance
  end type cell

  abstract interface
     function create_cell_test(this, p) result(result1)
       use coordinate_type
       import cell
       class(cell), intent(inout) :: this
       type(coordinate), intent(in) :: p
       logical :: result1
     end function create_cell_test

     function create_cell_distance(this, from, to) result(result1)
       use coordinate_type
       import cell
       class(cell), intent(inout) :: this
       type(coordinate), intent(in) :: from, to
       real :: result1
     end function create_cell_distance
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
     procedure, pass :: cell_distance => cell_distance_box
  end type cell_box_3d

contains

  function cell_test_box(this, p) result(result1)
    class(cell_box_3d), intent(inout) :: this
    type(coordinate), intent(in) :: p
    logical :: result1, b1, b2, b3, b4, b5, b6
    b1=.not. this%wallx_negative%surface_test(p)
    b2=this%wallx_positive%surface_test(p)
    b3=.not. this%wally_negative%surface_test(p)
    b4=this%wally_positive%surface_test(p)
    b5=.not. this%wallz_negative%surface_test(p)
    b6=this%wallz_positive%surface_test(p)

    if(b1 .and. b2 .and. b3 .and. b4 .and. b4 .and. b5 .and. b6) then
       this%hits=this%hits+1
       result1=.true.
    else
       result1=.false.
    end if
  end function cell_test_box

  function cell_distance_box(this, from, to) result(result1)
    class(cell_box_3d), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    real :: result1, distance
    real, dimension(5) :: distances
    integer :: i=1
    distances(1)=this%wallx_negative%surface_distance(from, to)
    distances(2)=this%wallx_positive%surface_distance(from, to)
    distances(3)=this%wally_negative%surface_distance(from, to)
    distances(4)=this%wally_positive%surface_distance(from, to)
    distances(5)=this%wallz_negative%surface_distance(from, to)
    distance=this%wallz_positive%surface_distance(from, to)

    do
       if(i==5) then
          exit
       else if(distances(i)<distance .and. distances(i)>0) then
          distance=distances(i)
       else if(distance<0 .and. distances(i)>0) then
          distance=distances(i)
       end if
       i=i+1
    end do
    result1=distance
  end function cell_distance_box

end module cell_type
