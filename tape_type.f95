module tape_type
  implicit none

  type :: tape
  end type tape

contains

  subroutine read_tape1(tape_name)
    character(*) :: tape_name
    integer :: ios, z
    z=1

    open(z, file=tape_name, status="old", action="read", iostat=ios)
    ! call read_begin(z, ios)

    do
       if(ios<0) exit
       ! call read_header(z, ios)
       ! call read_section(z, ios)
       ! print *, ""
    end do

    close(z)
  end subroutine read_tape1
end module tape_type
