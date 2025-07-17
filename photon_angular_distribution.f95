module photon_angular_distribution
  ! use tape_type
  implicit none

contains

  subroutine create_incoherent(incoherent, incoherent_function, n1, n2)
    real(kind(1.d0)), allocatable :: incoherent(:, :)
    real(kind(1.d0)), allocatable :: incoherent_function(:, :)
    integer, intent(in) :: n1, n2
  end subroutine create_incoherent

end module photon_angular_distribution
