module file_type
  use section_type
  implicit none

  type, abstract :: file
   contains
     procedure(create_file), deferred :: create
  end type file

  abstract interface
     subroutine create_file(this, z, ios, MF, MT)
       import file
       class(file), intent(inout) :: this
       integer :: z, ios, MF, MT
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

  subroutine create_mf23(this, z, ios, MF, MT)
    class(MF23), intent(inout) :: this
    integer :: z, ios, MF, MT
  end subroutine create_mf23

  subroutine create_mf27(this, z, ios, MF, MT)
    class(MF27), intent(inout) :: this
    integer :: z, ios, MF, MT
  end subroutine create_mf27

end module file_type
