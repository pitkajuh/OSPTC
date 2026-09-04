module post_processing
  use constants, only: elementary_charge
  implicit none

contains

  function coordinate_transform(v, dv, v0) result(s)
    real(kind(1.d0)), intent(in) :: v, dv, v0
    integer :: s
    s=int(v/dv)+abs(int(v0/dv))
  end function coordinate_transform

  function coordinate_inverse_transform(v, dv, v0) result(s)
    integer, intent(in) :: v
    real(kind(1.d0)), intent(in) :: dv, v0
    real(kind(1.d0)) :: s
    s=v*dv-abs(v0)
  end function coordinate_inverse_transform

  subroutine read_file(file_name, id, grid, dx, dy, dz, x0, y0, z0)
    character(*), intent(in) :: file_name
    integer, intent(in) :: id
    real(kind(1.d0)), intent(in) :: dx, dy, dz, x0, y0, z0
    real(kind(1.d0)), allocatable, dimension(:, :, :) :: grid
    integer :: io, ii, grid_x, grid_y, grid_z
    real(kind(1.d0)) :: x, y, z, energy, mass
    print *, "read file ", file_name
    open(newunit=ii, file=file_name, status="old", action="read")
    print *, "open file"
    x=0
    y=0
    z=0
    energy=0
    mass=0

    do
       read(ii, *, iostat=io) x, y, z, energy, mass
       if (is_iostat_end(io)) exit
       grid_x=coordinate_transform(x, dx, x0)
       grid_y=coordinate_transform(y, dy, y0)
       grid_z=coordinate_transform(z, dz, z0)
       print *, x, grid_x, coordinate_inverse_transform(grid_x, dx, x0)
       grid(grid_x, grid_y, grid_z)=grid(grid_x, grid_y, grid_z)+energy*elementary_charge/mass

       exit
    end do

    close(ii)
  end subroutine read_file

  subroutine post_process_results(directory, time_end, x0, x1, y0, y1, z0, z1)
    character(len=23), intent(in) :: directory
    integer, intent(in) :: time_end
    real(kind(1.d0)), intent(in) :: x0, x1, y0, y1, z0, z1
    real(kind(1.d0)), allocatable, dimension(:, :, :) :: grid
    character(len=32) :: temporary, file_name
    integer :: i, x, y, z, n
    real(kind(1.d0)) :: dx, dy, dz
    ! grid=create_grid(x0, x1, y0, y1, z0, z1)

    n=1000
    dx=(x1-x0)/n
    dy=(y1-y0)/n
    dz=(z1-z0)/n

    allocate(grid(n, n, n))
    i=1

    ! i=1
    ! do i=1, time_end
    !    do x=1, 200
    !       do y=1, 200
    !          do z=1, 200
    !          end do
    !       end do
    !    end do
       write(temporary, '(I0)') i
       temporary=trim(temporary)
       file_name=trim(directory//temporary)
       call read_file(file_name, i, grid, dx, dy, dz, x0, y0, z0)

    !    exit
    ! end do
    deallocate(grid)
  end subroutine post_process_results

end module post_processing
