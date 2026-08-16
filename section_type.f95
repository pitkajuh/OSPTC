module section_type
  implicit none

  type, abstract :: section
     real(kind(1.d0)), dimension(6, 3) :: header
     real(kind(1.d0)), allocatable :: records(:, :)
     integer :: n
   contains
     procedure, public :: read_section_header
     procedure, public :: read_section
     procedure, public :: skip_section
     procedure, public :: section_destructor
  end type section

  ! MF23

  type, extends(section) :: section_coherent_scattering
  end type section_coherent_scattering

  type, extends(section) :: section_incoherent_scattering
  end type section_incoherent_scattering

  type, extends(section) :: section_pair_formation
  end type section_pair_formation

  type, extends(section) :: section_photo_ionization
  end type section_photo_ionization

  ! MF27

  type, extends(section) :: section_coherent_factor
  end type section_coherent_factor

  type, extends(section) :: section_incoherent_function
  end type section_incoherent_function

  type, extends(section) :: section_imaginary_factor
  end type section_imaginary_factor

  type, extends(section) :: section_real_factor
  end type section_real_factor

contains

  subroutine section_destructor(this)
    class(section), intent(inout) :: this
    deallocate(this%records)
  end subroutine section_destructor

  subroutine skip_section(this, z, ios)
    class(section), intent(inout) :: this
    real(kind(1.d0)) :: v1, v2, v3, v4, v5, v6
    integer :: MAT, MF, MT, z, ios

    do
       read(z, '(6E11.0,I4,I2,I3)', iostat=ios) v1, v2, v3, v4, v5, v6, MAT, MF, MT
       if(MT==0) exit
    end do
  end subroutine skip_section

  subroutine read_section_header(this, z, ios, MF, MT)
    class(section), intent(inout) :: this
    real(kind(1.d0)) :: ZA, AWR
    integer :: L1, L2, N1, N2, MAT, MF, MT, z, ios, i

    read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT

    if(MF==0 .and. MT==0) return

    ! print *, ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
    this%header(1, 1)=ZA
    this%header(2, 1)=AWR
    this%header(3, 1)=L1
    this%header(4, 1)=L2
    this%header(5, 1)=N1
    this%header(6, 1)=N2

    do i=2, 3
       read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       ! print *, ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       this%header(1, i)=ZA
       this%header(2, i)=AWR
       this%header(3, i)=L1
       this%header(4, i)=L2
       this%header(5, i)=N1
       this%header(6, i)=N2
    end do
  end subroutine read_section_header

  subroutine read_section(this, z, ios, MF,  MT, n, records)
    class(section), intent(inout) :: this
    real(kind(1.d0)), allocatable :: records(:, :)
    real(kind(1.d0)) :: e1, v2, e2, v4, e3, v6
    integer :: MAT, MF, MT, z, ios, n, i
    i=1
    this%n=n

    do
       read(z, '(6E11.0,I4,I2,I3)', iostat=ios) e1, v2, e2, v4, e3, v6, MAT, MF, MT
       if(MT==0) exit
       ! print *, e1, v2, e2, v4, e3, v6, MAT, MF, MT
       if(e1==0.0_8 .and. v2==0.0_8) then
          records(1, i)=e2
          records(2, i)=v4
          records(1, i+1)=e3
          records(2, i+1)=v6
          i=i-1
          ! print *, "crcc", e2, v4, e3, v6
       else if(e1==e2 .or. v2==0.0_8) then
          ! Ionization rows have form
          ! 7117.00000 0.0 7117.00000 33116.3511 7220.00000 31869.8000
          ! values 7117.00000 0.0 are omitted.
          ! Same for pair production
          ! 1022000.00 0.0 1025120.00 3.02833E-8 1025233.25 3.25429E-8
          records(1, i)=e2
          records(2, i)=v4
          records(1, i+1)=e3
          records(2, i+1)=v6
          i=i-1
       else if(i+1>n) then
          records(1, i)=e1
          records(2, i)=v2
       else if(i+2>n) then
          records(1, i)=e1
          records(2, i)=v2
          records(1, i+1)=e2
          records(2, i+1)=v4
       else
          records(1, i)=e1
          records(2, i)=v2
          records(1, i+1)=e2
          records(2, i+1)=v4
          records(1, i+2)=e3
          records(2, i+2)=v6
          ! print *, "AA", e1, v2, e2, v4, e3, v6
       end if
       i=i+3
    end do
  end subroutine read_section
end module section_type
