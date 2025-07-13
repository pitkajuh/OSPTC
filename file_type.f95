module file_type
  use section_type
  implicit none

  type, abstract :: file
   contains
     procedure(create_file), deferred :: create
  end type file

  abstract interface
     subroutine create_file(this, z, ios)
       import file
       class(file), intent(inout) :: this
       integer :: z, ios
     end subroutine create_file
  end interface

  type, extends(file) :: MF23
     type(section_coherent_scattering) :: coherent_scattering
     type(section_incoherent_scattering) :: incoherent_scattering
     type(section_pair_formation) :: pair_formation
     type(section_photo_ionization) :: photo_ionization
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

  subroutine create_mf23(this, z, ios)
    class(MF23), intent(inout) :: this
    integer :: z, ios, MF, MT

    ! Skip 23501
    call this%coherent_scattering%skip_section(z, ios, MF, MT)
    print *, ""

    call this%coherent_scattering%read_section_header(z, ios, MF, MT)
    call this%coherent_scattering%read_section(z, ios, MF, MT)
    print *, ""

    call this%incoherent_scattering%read_section_header(z, ios, MF, MT)
    call this%incoherent_scattering%read_section(z, ios, MF, MT)
    print *, ""

    call this%pair_formation%read_section_header(z, ios, MF, MT)
    call this%pair_formation%read_section(z, ios, MF, MT)
    print *, ""

    ! Skip 23516
    call this%pair_formation%skip_section(z, ios, MF, MT)

    call this%pair_formation%read_section_header(z, ios, MF, MT)
    call this%pair_formation%read_section(z, ios, MF, MT)
    print *, ""

    ! Skip 23522
    call this%photo_ionization%skip_section(z, ios, MF, MT)

    do
       call this%photo_ionization%read_section_header(z, ios, MF, MT)
       if(MT==0 .and. MF==0) exit
       call this%photo_ionization%read_section(z, ios, MF, MT)
       print *, ""
    end do
  end subroutine create_mf23

  subroutine create_mf27(this, z, ios)
    class(MF27), intent(inout) :: this
    integer :: z, ios, MF, MT

    call this%coherent_factor%read_section_header(z, ios, MF, MT)
    call this%coherent_factor%read_section(z, ios, MF, MT)
    print *, ""

    call this%incoherent_function%read_section_header(z, ios, MF, MT)
    call this%incoherent_function%read_section(z, ios, MF, MT)
    print *, ""

    call this%imaginary_factor%read_section_header(z, ios, MF, MT)
    call this%imaginary_factor%read_section(z, ios, MF, MT)
    print *, ""

    call this%real_factor%read_section_header(z, ios, MF, MT)
    call this%real_factor%read_section(z, ios, MF, MT)
    print *, ""

  end subroutine create_mf27

end module file_type
