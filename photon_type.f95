module photon_type
  use random
  use coodinate_type
  implicit none

  type :: photon
     real(kind(1.d0)) :: energy
     type(coodinate) :: direction
     type(coodinate) :: origin
     type(coodinate) :: to
     type(coodinate) :: mfp

     real(kind(1.d0)), dimension(6, 4) :: header
     real(kind(1.d0)), allocatable :: coherent_A(:, :)
     real(kind(1.d0)), allocatable :: incoherent_A(:, :)
     integer, allocatable :: sizes(:)
     integer :: n, Ax, Ax1
     type(MF23) :: mf23
     type(MF27) :: mf27
  end type photon

contains

  ! subroutine read_tape(this, tape_name)
  !   type(tape) :: this
  !   character(*) :: tape_name
  !   integer :: ios, z, i
  !   real(kind(1.d0)) :: emax, emin
  !   z=1
  ! end subroutine read_tape



end module photon_type
