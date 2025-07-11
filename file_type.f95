module file_type
  implicit none

  type :: file
     real, dimension(6, 3) :: header
     real, allocatable :: section(:)
     integer :: to=2
  end type file

contains

  subroutine read_file_header(this, z, ios, MF, MT)
    type(file) :: this
    integer :: z
    integer :: ios
    integer :: n=1
    integer :: to
    real :: ZA, AWR
    integer :: L1, L2, N1, N2, MAT, MF, MT
    ! n=1
    ! print *, "file header read"
    read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
    ! print *, ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
    if(MF==0 .and. MT==0) then
       print *, "change file"
       return
    end if
    ! if(MF==0 .and. this%to==2) then
    !    ! Change file
    !    print *, "change file", MF, MT
    !    return
    !    this%to=3
    ! else
       print *, ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       this%header(1, n)=ZA
       this%header(2, n)=AWR
       this%header(3, n)=L1
       this%header(4, n)=L2
       this%header(5, n)=N1
       this%header(6, n)=N2
    ! end if

    do
       read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT

       ! if(MF==0 .and. MT==0) exit
       print *, ZA, AWR, L1, L2, N1, N2, MAT, MF, MT, n, this%to
       this%header(1, n)=ZA
       this%header(2, n)=AWR
       this%header(3, n)=L1
       this%header(4, n)=L2
       this%header(5, n)=N1
       this%header(6, n)=N2
       if(n==this%to) exit
       n=n+1
    end do
    n=1
  end subroutine read_file_header

  subroutine read_section(this, z, ios, MF,  MT)
    type(file) :: this
    integer :: z
    integer :: n=0
    integer :: ios
    real :: v1, v2, v3, v4, v5, v6
    integer :: L1, L2, N1, N2, MAT, MF, MT
    ! n=0
    ! print *, "read sectin"

    ! read(z, '(6E11.0,I4,I2,I3)', iostat=ios) v1, v2, v3, v4, v5, v6, MAT, MF, MT
    ! print *, v1, v2, v3, v4, v5, v6, MAT, MF, MT
    ! if(MF==0 .and. MT==0) return

    do
       read(z, '(6E11.0,I4,I2,I3)', iostat=ios) v1, v2, v3, v4, v5, v6, MAT, MF, MT
       ! print *, v1, v2, v3, v4, v5, v6, MAT, MF, MT
       ! if(MT==0) exit
       if(MT==0) then
          ! print *, v1, v2, v3, v4, v5, v6, MAT, MF, MT, "exit"
          exit
       end if
       ! print *, v1, v2, v3, v4, v5, v6, MAT, MF, MT
       n=n+1
    end do
    ! print *, n+3
    ! real, dimension(2) :: a1
    ! print *, "end sectin"
    ! print *, v1, v2, v3, v4, v5, v6, MAT, MF, MT
  end subroutine read_section

end module file_type
