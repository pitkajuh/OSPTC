module cell_type
  use random
  use coordinate_type
  use surface_type
  use material_type
  use photon_type
  use constants, only: elementary_charge, pi
  implicit none

  type :: cells
     class(cell), allocatable :: cell_array
  end type cells

  type, abstract :: cell
     character :: name
     real(kind(1.d0)) :: accumulated_energy
     class(material), pointer:: cell_material
   contains
     procedure(create_cell_test), deferred :: cell_test
     procedure(create_cell_distance), deferred :: cell_distance
     procedure(create_initial_position), deferred :: random_initial_position
     procedure(create_create), deferred :: create
     procedure(calculate_dose), deferred :: get_dose
  end type cell

  abstract interface

     function calculate_dose(this) result(dose)
       import cell
       class(cell), intent(inout) :: this
       real(kind(1.d0)) :: dose
     end function calculate_dose

     subroutine create_create(this, x0, x1, y0, y1, z0, z1)
       import cell
       class(cell), intent(inout) :: this
       real(kind(1.d0)), intent(in) :: x0, x1, y0, y1, z0, z1
     end subroutine create_create

     function create_cell_test(this, p, energy) result(result1)
       import coordinate
       import cell
       class(cell), intent(inout) :: this
       type(coordinate), intent(in) :: p
       real(kind(1.d0)), intent(in) :: energy
       logical :: result1
     end function create_cell_test

     function create_cell_distance(this, from, direction) result(result1)
       import coordinate
       import cell
       class(cell), intent(inout) :: this
       type(coordinate), intent(in) :: from, direction
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
     procedure, pass :: get_dose => get_box_3d_dose
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
     procedure, pass :: get_dose => get_cylinder_truncated_z_dose
  end type cell_cylinder_truncated_z

contains

  function get_cylinder_truncated_z_dose(this) result(dose)
    class(cell_cylinder_truncated_z), intent(inout) :: this
    real(kind(1.d0)) :: dose, mass, volume
    ! pi*r*2*(z1-z0)
    volume=pi*this%surface_cylinder%v*this%surface_cylinder%v&
         *(this%wallz_positive%v-this%wallz_negative%v)
    mass=this%cell_material%density*volume
    dose=this%accumulated_energy*elementary_charge/mass
  end function get_cylinder_truncated_z_dose

  function get_box_3d_dose(this) result(dose)
    class(cell_box_3d), intent(inout) :: this
    real(kind(1.d0)) :: dose, mass, volume
    volume=(this%wallx_positive%v-this%wallx_negative%v)*&
         (this%wally_positive%v-this%wally_negative%v)*&
         (this%wallz_positive%v-this%wallz_negative%v)
    mass=this%cell_material%density*volume
    dose=this%accumulated_energy*elementary_charge/mass
    mass=this%cell_material%density*volume
  end function get_box_3d_dose

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

  function cell_test_box(this, p, energy) result(result1)
    class(cell_box_3d), intent(inout) :: this
    type(coordinate), intent(in) :: p
    real(kind(1.d0)), intent(in) :: energy
    logical :: result1, b1, b2, b3, b4, b5, b6
    b1=.not. this%wallx_negative%surface_test(p)
    b2=this%wallx_positive%surface_test(p)
    b3=.not. this%wally_negative%surface_test(p)
    b4=this%wally_positive%surface_test(p)
    b5=.not. this%wallz_negative%surface_test(p)
    b6=this%wallz_positive%surface_test(p)
    result1=.false.

    if(b1 .and. b2 .and. b3 .and. b4 .and. b4 .and. b5 .and. b6) then
       !!omp atomic update
       ! this%accumulated_energy=this%accumulated_energy+energy
       result1=.true.
    end if
  end function cell_test_box

  function cell_distance_box(this, from, direction) result(result1)
    class(cell_box_3d), intent(inout) :: this
    type(coordinate), intent(in) :: from, direction
    real(kind(1.d0)) :: result1, distance
    integer :: i
    real(kind(1.d0)), dimension(5) :: distances
    distances(1)=this%wallx_negative%surface_distance(from, direction)
    distances(2)=this%wallx_positive%surface_distance(from, direction)
    distances(3)=this%wally_negative%surface_distance(from, direction)
    distances(4)=this%wally_positive%surface_distance(from, direction)
    distances(5)=this%wallz_negative%surface_distance(from, direction)
    distance=this%wallz_positive%surface_distance(from, direction)

    ! Find the distance to the surface that is the closest one, i.e.
    ! find the smallest value. The distance must be >0.
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

  function cell_test_cylinder_z(this, p, energy) result(result1)
    class(cell_cylinder_truncated_z), intent(inout) :: this
    type(coordinate), intent(in) :: p
    real(kind(1.d0)), intent(in) :: energy
    logical :: result1, b1, b2, b3
    b1=this%surface_cylinder%surface_test(p)
    b2=this%wallz_positive%surface_test(p)
    b3=.not. this%wallz_negative%surface_test(p)
    result1=.false.

    if(b1 .and. b2 .and. b3) then
       !!omp atomic update
       ! this%accumulated_energy=this%accumulated_energy+energy
       result1=.true.
    end if
  end function cell_test_cylinder_z

  function cell_distance_cylinder_z(this, from, direction) result(result1)
    class(cell_cylinder_truncated_z), intent(inout) :: this
    type(coordinate), intent(in) :: from, direction
    real(kind(1.d0)) :: result1, distance
    integer :: i
    real(kind(1.d0)), dimension(2) :: distances
    distances=(/this%wallz_negative%surface_distance(from, direction),  this%wallz_positive%surface_distance(from, direction)/)
    distance=this%surface_cylinder%surface_distance(from, direction)
    ! Find the distance to the surface that is the closest one,
    ! i.e. find the smallest value. The distance must be >0.

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

  function cell_search(cell_list, n, mfp, energy) result(cell_index)
    class(cells), intent(inout), allocatable :: cell_list(:)
    type(coordinate), intent(in) :: mfp
    real(kind(1.d0)), intent(in) :: energy
    integer, intent(in) :: n
    integer :: i, cell_index
    logical :: cell_hit
    cell_index=0

    do i=1, n
       cell_hit=cell_list(i)%cell_array%cell_test(mfp, energy)

       if(cell_hit .eqv. .true.) then
          cell_index=i
          exit
       end if
    end do
  end function cell_search

end module cell_type
