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
     function create_pdf(this) result(ph)
       import radionuclide
       import photon
       class(radionuclide), intent(inout) :: this
       type(photon) :: ph
     end function create_pdf
  end interface

  type, extends(radionuclide) :: co_60
   contains
     procedure, pass :: pdf => pdf_co_60
  end type co_60

contains

  function pdf_co_60(this) result(ph)
    class(co_60), intent(inout) :: this
    type(photon) :: ph
    type(coordinate) :: origin
    origin=coordinate(1.0_8, 1.0_8, 1.0_8)

    if(std_uniform_distribution()<=0.85) then
       ! call create_photon(ph, 1173E3_8, origin)
       call create_photon(ph, 1173E4_8, origin)
    else
       ! call create_photon(ph, 1332E3_8, origin)
       call create_photon(ph, 1332E4_8, origin)
    end if
  end function pdf_co_60

end module radionuclide_type
