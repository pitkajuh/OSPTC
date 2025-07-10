module file_type

  implicit none

  type :: file
     real, dimension(6, 3) :: header
  end type file

contains

  subroutine read_file_header(z, ios)
    implicit none
    type(file) :: this
    integer :: z
    integer :: ios
    integer :: n
    real(kind(1.d0)) :: ZA, AWR
    integer :: L1, L2, N1, N2, MAT, MF, MT
    n=1

    do
       read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       ! print *, ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       this%header(1, n)=ZA
       this%header(2, n)=AWR
       this%header(3, n)=L1
       this%header(4, n)=L2
       this%header(5, n)=N1
       this%header(6, n)=N2
       if(n==3) exit
       n=n+1
    end do

  end subroutine read_file_header


end module file_type
