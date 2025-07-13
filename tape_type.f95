module tape_type
  use file_type
  implicit none

  type :: tape
     real, dimension(6, 4) :: header
     type(MF23) :: mf23
     type(MF27) :: mf27
  end type tape

contains

  subroutine read_tape1(this, tape_name)
    type(tape) :: this
    character(*) :: tape_name
    integer :: ios, z
    z=1

    open(z, file=tape_name, status="old", action="read", iostat=ios)
    call read_begin1(this, z, ios)
    call this%mf23%create(z, ios)
    call this%mf27%create(z, ios)
    close(z)
  end subroutine read_tape1

  subroutine read_begin1(this, z, ios)
    type(tape) :: this
    integer :: z, ios, i
    character(75) :: line

    real(kind(1.d0)) :: ZA, AWR
    integer :: L1, L2, N1, N2, MAT, MF, MT, MT1, MF1

    read(z, *, iostat=ios) line

    do i=1, 4
       read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       this%header(1, i)=ZA
       this%header(2, i)=AWR
       this%header(3, i)=L1
       this%header(4, i)=L2
       this%header(5, i)=N1
       this%header(6, i)=N2
    end do

    do
       read(z, '(A65,I1)', iostat=ios) line, N1
       if(N1==3)  exit
    end do

    do
       read(z, '(I36,I10,I10,I10)', iostat=ios) N1, MAT, MF, MT
       if(MT/=3) exit
       print *, N1, MAT, MF, MT
    end do

    read(z, '(I36,I10,I10,I10)', iostat=ios) N1, MAT, MF, MT

  end subroutine read_begin1
end module tape_type
