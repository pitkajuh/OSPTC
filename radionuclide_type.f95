module radionuclide_type
  use random
  use photon_type
  use coordinate_type
  implicit none

  type, abstract :: radionuclide
     integer :: activity
     ! real(kind(1.d0)) :: half_life
   contains
     procedure(create_pdf), deferred :: pdf
  end type radionuclide

  abstract interface
     subroutine create_pdf(this, ph)
       import radionuclide
       import photon
       class(radionuclide), intent(inout) :: this
       type(photon), intent(inout) :: ph
     end subroutine create_pdf
  end interface

  type, extends(radionuclide) :: co_60
   contains
     procedure, pass :: pdf => pdf_co_60
  end type co_60

contains

  subroutine pdf_co_60(this, ph)
    class(co_60), intent(inout) :: this
    type(photon), intent(inout) :: ph
    type(coordinate) :: origin
    real(kind(1.d0)) :: energy
    energy=0.0_8
    origin=coordinate(1.0_8, 1.0_8, 1.0_8)

    if(std_uniform_distribution()<=0.85) then
       ! call create_photon(ph, 1173E3_8, origin)
       energy=1173E4_8
       call create_photon(ph, energy, origin)
    else
       ! call create_photon(ph, 1332E3_8, origin)
       energy=1332E4_8
       call create_photon(ph, energy, origin)
    end if
  end subroutine pdf_co_60

end module radionuclide_type
