module file_type
  use section_type
  implicit none

  type, abstract :: file
   contains
     procedure(create_file), deferred :: create
  end type file

  abstract interface
     subroutine create_file(this, z, ios, sizes, n)
       import file
       class(file), intent(inout) :: this
       integer :: z, ios, n
       integer, allocatable :: sizes(:)
     end subroutine create_file
  end interface

  type, extends(file) :: MF23
     type(section_coherent_scattering) :: coherent_scattering
     type(section_incoherent_scattering) :: incoherent_scattering
     type(section_pair_formation) :: pair_formation_elec
     type(section_pair_formation) :: pair_formation_nuc
     type(section_photo_ionization), allocatable :: photo_ionization(:)
   contains
     procedure, pass :: create => create_mf23
  end type MF23

  type, extends(file) :: MF27
     type(section_coherent_factor) :: coherent_factor
     type(section_incoherent_function) :: incoherent_function
     type(section_imaginary_factor) :: imaginary_factor
     type(section_real_factor) :: real_factor
   contains
     procedure, pass :: create => create_mf27
  end type MF27

contains

  subroutine create_mf23(this, z, ios, sizes, n)
    class(MF23), intent(inout) :: this
    integer :: z, ios, MF, MT, n, i
    integer, allocatable :: sizes(:)

    ! Skip 23501
    call this%coherent_scattering%skip_section(z, ios)
    print *, ""

    allocate(this%coherent_scattering%records(10, sizes(1)))
    call this%coherent_scattering%read_section_header(z, ios, MF, MT)
    call this%coherent_scattering%read_section(z, ios, MF, MT, sizes(1))
    print *, ""

    allocate(this%incoherent_scattering%records(10, sizes(2)))
    call this%incoherent_scattering%read_section_header(z, ios, MF, MT)
    call this%incoherent_scattering%read_section(z, ios, MF, MT, sizes(2))
    print *, ""

    allocate(this%pair_formation_elec%records(10, sizes(3)))
    call this%pair_formation_elec%read_section_header(z, ios, MF, MT)
    call this%pair_formation_elec%read_section(z, ios, MF, MT, sizes(3))
    print *, ""

    ! Skip 23516
    call this%pair_formation_nuc%skip_section(z, ios)

    allocate(this%pair_formation_nuc%records(10, sizes(4)))
    call this%pair_formation_nuc%read_section_header(z, ios, MF, MT)
    call this%pair_formation_nuc%read_section(z, ios, MF, MT, sizes(4))
    print *, ""

    ! Skip 23522
    allocate(this%photo_ionization(n-8))
    call this%photo_ionization(1)%skip_section(z, ios)
    ! print *, "@", n-9, n

    i=5
    print *, n-8
    do
       call this%photo_ionization(i-4)%read_section_header(z, ios, MF, MT)
       if(MT==0 .and. MF==0) exit
       ! allocate(this%photo_ionization(i-4))
       call this%photo_ionization(i-4)%read_section(z, ios, MF, MT, sizes(i))
       print *, "", i, i-4, n-8
       i=i+1
    end do
  end subroutine create_mf23

  subroutine create_mf27(this, z, ios, sizes, n)
    class(MF27), intent(inout) :: this
    integer :: z, ios, MF, MT, n
    integer, allocatable :: sizes(:)

    ! allocate(this%coherent_factor%records(10, sizes(n-3))
    call this%coherent_factor%read_section_header(z, ios, MF, MT)
    call this%coherent_factor%read_section(z, ios, MF, MT, sizes(n-3))
    print *, ""

    ! allocate(this%incoherent_function%records(10, sizes(n-2))
    call this%incoherent_function%read_section_header(z, ios, MF, MT)
    call this%incoherent_function%read_section(z, ios, MF, MT, sizes(n-2))
    print *, ""

    ! allocate(this%imaginary_factor%records(10, sizes(n-1))
    call this%imaginary_factor%read_section_header(z, ios, MF, MT)
    call this%imaginary_factor%read_section(z, ios, MF, MT, sizes(n-1))
    print *, ""

    ! allocate(this%real_factor%records(10, sizes(n)))
    call this%real_factor%read_section_header(z, ios, MF, MT)
    call this%real_factor%read_section(z, ios, MF, MT, sizes(n))
    print *, ""

  end subroutine create_mf27
end module file_type
