module cell_type
  use random
  use coordinate_type
  use surface_type
  use material_type
  use photon_type
  implicit none

  type :: cells
     class(cell), allocatable :: cell_array
  end type cells

  type, abstract :: cell
     character :: name
     integer :: hits
     class(material), allocatable :: cell_material
   contains
     procedure(create_cell_test), deferred :: cell_test
     procedure(create_cell_distance), deferred :: cell_distance
     procedure(create_initial_position), deferred :: random_initial_position
     procedure(create_create), deferred :: create
  end type cell

  abstract interface

     subroutine create_create(this, x0, x1, y0, y1, z0, z1)
       import cell
       class(cell), intent(inout) :: this
       real(kind(1.d0)), intent(in) :: x0, x1, y0, y1, z0, z1
     end subroutine create_create

     function create_cell_test(this, p) result(result1)
       import coordinate
       import cell
       class(cell), intent(inout) :: this
       type(coordinate), intent(in) :: p
       logical :: result1
     end function create_cell_test

     function create_cell_distance(this, from, to) result(result1)
       import coordinate
       import cell
       class(cell), intent(inout) :: this
       type(coordinate), intent(in) :: from, to
       real(kind(1.d0)) :: result1
     end function create_cell_distance

     function create_initial_position(this) result(result1)
       import coordinate
       import cell
       class(cell), intent(inout) :: this
       type(coordinate) :: result1
     end function create_initial_position
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
     procedure, pass :: random_initial_position => cell_initial_position_box
     procedure, pass :: create => create_cell_box_3d
  end type cell_box_3d

  type, extends(cell) :: cell_cylinder_truncated_z
     type(cylinder) :: surface_cylinder
     type(planez) :: wallz_negative
     type(planez) :: wallz_positive
   contains
     procedure, pass :: cell_test => cell_test_cylinder_z
     procedure, pass :: cell_distance => cell_distance_cylinder_z
     procedure, pass :: random_initial_position => cell_initial_position_cylinder_z
     procedure, pass :: create => create_cell_cylinder_truncated_z
  end type cell_cylinder_truncated_z

contains

  subroutine create_cell_box_3d(this, x0, x1, y0, y1, z0, z1)
    class(cell_box_3d), intent(inout) :: this
    real(kind(1.d0)), intent(in) :: x0, x1, y0, y1, z0, z1
    call create_planex(this%wallx_negative, x0)
    call create_planex(this%wallx_positive, x1)
    call create_planey(this%wally_negative, y0)
    call create_planey(this%wally_positive, y1)
    call create_planez(this%wallz_negative, z0)
    call create_planez(this%wallz_positive, z1)
  end subroutine create_cell_box_3d

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
    result1=.false.

    if(b1 .and. b2 .and. b3 .and. b4 .and. b4 .and. b5 .and. b6) then
       this%hits=this%hits+1
       result1=.true.
    end if
  end function cell_test_box

  function cell_distance_box(this, from, to) result(result1)
    class(cell_box_3d), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    real(kind(1.d0)) :: result1, distance
    integer :: i
    real(kind(1.d0)), dimension(5) :: distances
    distances(1)=this%wallx_negative%surface_distance(from, to)
    distances(2)=this%wallx_positive%surface_distance(from, to)
    distances(3)=this%wally_negative%surface_distance(from, to)
    distances(4)=this%wally_positive%surface_distance(from, to)
    distances(5)=this%wallz_negative%surface_distance(from, to)
    distance=this%wallz_positive%surface_distance(from, to)

    ! Find the distance to the surface that is the closest one, i.e. find the smallest value. The distance must be >0.
    do i=1, 5
       if(distances(i)<distance .and. distances(i)>0) then
          distance=distances(i)
       else if(distance<0 .and. distances(i)>0) then
          distance=distances(i)
       end if
    end do
    result1=distance
  end function cell_distance_box

  function cell_initial_position_box(this) result(result1)
    class(cell_box_3d), intent(inout) :: this
    type(coordinate) :: result1
    result1%x=rng(this%wallx_negative%v, this%wallx_positive%v)
    result1%y=rng(this%wally_negative%v, this%wally_positive%v)
    result1%z=rng(this%wallz_negative%v, this%wallz_positive%v)
  end function cell_initial_position_box

  subroutine create_cell_cylinder_truncated_z(this, x0, x1, y0, y1, z0, z1)
    class(cell_cylinder_truncated_z), intent(inout) :: this
    real(kind(1.d0)), intent(in) :: x0, x1, y0, y1, z0, z1
    type(coordinate) :: centered_at
    centered_at=coordinate(y1, z0, z1)
    call create_cylinder(this%surface_cylinder, x0, centered_at)
    call create_planez(this%wallz_negative, x1)
    call create_planez(this%wallz_positive, y0)
  end subroutine create_cell_cylinder_truncated_z

  function cell_test_cylinder_z(this, p) result(result1)
    class(cell_cylinder_truncated_z), intent(inout) :: this
    type(coordinate), intent(in) :: p
    logical :: result1, b1, b2, b3
    b1=this%surface_cylinder%surface_test(p)
    b2=this%wallz_positive%surface_test(p)
    b3=.not. this%wallz_negative%surface_test(p)
    result1=.false.

    if(b1 .and. b2 .and. b3) then
       this%hits=this%hits+1
       result1=.true.
    end if
  end function cell_test_cylinder_z

  function cell_distance_cylinder_z(this, from, to) result(result1)
    class(cell_cylinder_truncated_z), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    real(kind(1.d0)) :: result1, distance
    integer :: i
    real(kind(1.d0)), dimension(2) :: distances
    distances=(/this%wallz_negative%surface_distance(from, to),  this%wallz_positive%surface_distance(from, to)/)
    distance=this%surface_cylinder%surface_distance(from, to)

    ! Find the distance to the surface that is the closest one,
    !i.e. find the smallest value. The distance must be >0.
    do i=1, 2
       if(distances(i)<distance .and. distances(i)>0) then
          distance=distances(i)
       else if(distance<0 .and. distances(i)>0) then
          distance=distances(i)
       end if
    end do
    result1=distance
  end function cell_distance_cylinder_z

  function cell_initial_position_cylinder_z(this) result(result1)
    class(cell_cylinder_truncated_z), intent(inout) :: this
    type(coordinate) :: result1
    real(kind(1.d0)) :: radial, azimuthal_angle
    radial=this%surface_cylinder%v*std_uniform_distribution()**0.5
    azimuthal_angle=2*3.14159265*std_uniform_distribution()

    result1%x=radial*cos(azimuthal_angle)
    result1%y=radial*sin(azimuthal_angle)
    result1%z=this%wallz_negative%v+(this%wallz_positive%v-this%wallz_negative%v)*std_uniform_distribution()
  end function cell_initial_position_cylinder_z

  subroutine cell_search(cell_list, n, ph)
    class(cells), intent(inout), allocatable :: cell_list(:)
    type(photon), intent(inout) :: ph
    integer, intent(in) :: n
    type(coordinate) :: new_location
    integer :: i

    do i=1, n
       print *, cell_list(i)%cell_array%name
    end do
  end subroutine cell_search

end module cell_type
