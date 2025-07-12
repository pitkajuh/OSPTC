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
     procedure(create_initial_position), deferred :: get_initial_position
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

     function create_initial_position(this) result(result1)
       use coordinate_type
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
     procedure, pass :: get_initial_position => cell_box_initial_position
  end type cell_box_3d

  type, extends(cell) :: cell_cylinder_truncated_z
     type(cylinder) :: surface_cylinder
     type(planez) :: wallz_negative
     type(planez) :: wallz_positive
   contains
     procedure, pass :: cell_test => cell_test_cylinder_z
     procedure, pass :: cell_distance => cell_distance_cylinder_z
     procedure, pass :: get_initial_position => cell_cylinder_z_initial_position
  end type cell_cylinder_truncated_z

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

    ! Find the distance to the surface that is the closest one, i.e. find the smallest value. The distance must be >0.
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

  function cell_box_initial_position(this) result(result1)
    class(cell_box_3d), intent(inout) :: this
    type(coordinate) :: result1
    result1%x=rng(this%wallx_negative%value1, this%wallx_positive%value1)
    result1%y=rng(this%wally_negative%value1, this%wally_positive%value1)
    result1%z=rng(this%wallz_negative%value1, this%wallz_positive%value1)
  end function cell_box_initial_position

  function cell_test_cylinder_z(this, p) result(result1)
    class(cell_cylinder_truncated_z), intent(inout) :: this
    type(coordinate), intent(in) :: p
    logical :: result1, b1, b2, b3
    b1=.not. this%surface_cylinder%surface_test(p)
    b2=this%wallz_positive%surface_test(p)
    b3=.not. this%wallz_negative%surface_test(p)

    if(b1 .and. b2 .and. b3) then
       this%hits=this%hits+1
       result1=.true.
    else
       result1=.false.
    end if
  end function cell_test_cylinder_z

  function cell_distance_cylinder_z(this, from, to) result(result1)
    class(cell_cylinder_truncated_z), intent(inout) :: this
    type(coordinate), intent(in) :: from, to
    real :: result1, distance
    real, dimension(2) :: distances
    integer :: i=1
    distances(1)=this%wallz_negative%surface_distance(from, to)
    distances(2)=this%wallz_positive%surface_distance(from, to)
    distance=this%surface_cylinder%surface_distance(from, to)

    ! ! Find the distance to the surface that is the closest one, i.e. find the smallest value. The distance must be >0.
    do
       if(i==2) then
          exit
       else if(distances(i)<distance .and. distances(i)>0) then
          distance=distances(i)
       else if(distance<0 .and. distances(i)>0) then
          distance=distances(i)
       end if
       i=i+1
    end do
    result1=distance
  end function cell_distance_cylinder_z

  function cell_cylinder_z_initial_position(this) result(result1)
    class(cell_cylinder_truncated_z), intent(inout) :: this
    type(coordinate) :: result1
    real :: radial, azimuthal_angle
    radial=this%surface_cylinder%value1*std_uniform_distribution()**0.5
    azimuthal_angle=2*3.14159265*std_uniform_distribution()

    result1%x=radial*cos(azimuthal_angle)
    result1%y=radial*sin(azimuthal_angle)
    result1%z=this%wallz_negative%value1+(this%wallz_positive%value1-this%wallz_negative%value1)*std_uniform_distribution()
  end function cell_cylinder_z_initial_position

end module cell_type
