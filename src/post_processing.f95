module post_processing
  implicit none

contains

  function create_grid_coordinate(v, dv, n, v0) result(s)
    real(kind(1.d0)), intent(in) :: v, dv, v0
    integer, intent(in) :: n
    integer :: s, a
    ! int(v*n/dv)
    ! if(v<0) then
       ! a=abs(int(v0/dv))
       ! end if
       ! print *, int(v/dv), abs(int(v0/dv))
    s=int(v/dv)+abs(int(v0/dv))
  end function create_grid_coordinate

  subroutine read_file(file_name, id, grid, dx, dy, dz, n, x0, y0, z0)
    character(*), intent(in) :: file_name
    integer, intent(in) :: id, n
    real(kind(1.d0)), intent(in) :: dx, dy, dz, x0, y0, z0
    real(kind(1.d0)), allocatable, dimension(:, :, :) :: grid
    integer :: io, ii
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
       print *, x, y, z, energy, mass!, create_grid_coordinate(x, dx, n, x0)
       print *, x, dx, int(x*n/dx)
       print *, create_grid_coordinate(x, dx, n, x0), create_grid_coordinate(y, dy, n, y0), create_grid_coordinate(z, dz, n, z0)

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
       call read_file(file_name, i, grid, dx, dy, dz, n, x0, y0, z0)

    !    exit
    ! end do
    deallocate(grid)
  end subroutine post_process_results

end module post_processing
