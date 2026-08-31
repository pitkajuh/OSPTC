module post_processing
  implicit none

contains

  function create_grid(x0, x1, y0, y1, z0, z1) result(grid)
    real(kind(1.d0)), intent(in) :: x0, x1, y0, y1, z0, z1
    real(kind(1.d0)) :: dx, dy, dz
    integer :: nx, ny, nz, dt
    real(kind(1.d0)), allocatable, dimension(:, :, :) :: grid
    dt=100
    nx=(x1-x0)*dt
    ny=(y1-y0)*dt
    nz=(z1-z0)*dt
    allocate(grid(nx, ny, nz))

  end function create_grid

  subroutine read_file(file_name, id)
    character(*), intent(in) :: file_name
    integer, intent(in) :: id
    open(id, file=file_name, status="old", action="read")

    ! do

    ! end do

    close(id)
  end subroutine read_file

  subroutine post_process_results(directory, time_end, x0, x1, y0, y1, z0, z1)
    character(len=23), intent(in) :: directory
    integer, intent(in) :: time_end
    real(kind(1.d0)), intent(in) :: x0, x1, y0, y1, z0, z1
    real(kind(1.d0)), allocatable, dimension(:, :, :) :: grid
    character(len=32) :: temporary, file_name
    integer :: i, x, y, z
    grid=create_grid(x0, x1, y0, y1, z0, z1)

    do i=1, time_end
    !    do x=1, 200
    !       do y=1, 200
    !          do z=1, 200
    !          end do
    !       end do
    !    end do
       write(temporary, '(I0)') i
       temporary=trim(temporary)
       file_name=trim(directory//temporary)
       call read_file(file_name, i)
       print *, file_name
    end do
    deallocate(grid)
  end subroutine post_process_results

end module post_processing
