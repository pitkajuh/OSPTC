module section_type
  implicit none

  type, abstract :: section
     real, dimension(6, 3) :: header
   contains
     procedure, public :: read_section_header
     procedure, public :: read_section
     procedure, public :: skip_section
  end type section

  ! MF23

  type, extends(section) :: section_coherent_scattering
   contains
  end type section_coherent_scattering

  type, extends(section) :: section_incoherent_scattering
   contains
  end type section_incoherent_scattering

  type, extends(section) :: section_pair_formation
   contains
  end type section_pair_formation

  type, extends(section) :: section_photo_ionization
   contains
  end type section_photo_ionization

  ! MF27

  type, extends(section) :: section_coherent_factor
   contains
  end type section_coherent_factor

  type, extends(section) :: section_incoherent_function
   contains
  end type section_incoherent_function

  type, extends(section) :: section_imaginary_factor
   contains
  end type section_imaginary_factor

  type, extends(section) :: section_real_factor
   contains
  end type section_real_factor

contains

  subroutine skip_section(this, z, ios, MF, MT)
    class(section), intent(inout) :: this
    real :: v1, v2, v3, v4, v5, v6
    integer :: L1, L2, N1, N2, MAT, MF, MT, z, ios

    do
       read(z, '(6E11.0,I4,I2,I3)', iostat=ios) v1, v2, v3, v4, v5, v6, MAT, MF, MT
       if(MT==0) exit
    end do
  end subroutine skip_section

  subroutine read_section_header(this, z, ios, MF, MT)
    class(section), intent(inout) :: this
    real :: ZA, AWR
    integer :: L1, L2, N1, N2, MAT, MF, MT, z, ios
    integer :: n=1
    integer :: to=2

    read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT

    if(MF==0 .and. MT==0) return

    print *, ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
    this%header(1, n)=ZA
    this%header(2, n)=AWR
    this%header(3, n)=L1
    this%header(4, n)=L2
    this%header(5, n)=N1
    this%header(6, n)=N2

    do
       read(z, '(2E11.0,4I11,I4,I2,I3,I5)', iostat=ios) ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       print *, ZA, AWR, L1, L2, N1, N2, MAT, MF, MT
       this%header(1, n)=ZA
       this%header(2, n)=AWR
       this%header(3, n)=L1
       this%header(4, n)=L2
       this%header(5, n)=N1
       this%header(6, n)=N2
       if(n==to) exit
       n=n+1
    end do
    n=1
  end subroutine read_section_header

  subroutine read_section(this, z, ios, MF,  MT)
    class(section), intent(inout) :: this
    real :: v1, v2, v3, v4, v5, v6
    integer :: L1, L2, N1, N2, MAT, MF, MT, z, ios

    do
       read(z, '(6E11.0,I4,I2,I3)', iostat=ios) v1, v2, v3, v4, v5, v6, MAT, MF, MT
       if(MT==0) exit
    end do
  end subroutine read_section
end module section_type
