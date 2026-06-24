module radionuclide_type
  use random
  implicit none

  type, abstract :: radionuclide
     integer :: activity
     real(kind(1.d0)) :: half_life, max_energy
   contains
     procedure(create_pdf), deferred :: pdf
  end type radionuclide

  abstract interface
     function create_pdf(this) result(pdf1)
       import radionuclide
       class(radionuclide), intent(inout) :: this
       real(kind(1.d0)) :: pdf1
     end function create_pdf
  end interface

  type, extends(radionuclide) :: co_60
   contains
     procedure, pass :: pdf => pdf_co_60
  end type co_60

contains

  function pdf_co_60(this) result(pdf1)
    class(co_60), intent(inout) :: this
    real(kind(1.d0)) :: pdf1
    if(std_uniform_distribution()<=0.85) then
       pdf1=1173E3
    else
       pdf1=1332E3
    end if
  end function pdf_co_60

end module radionuclide_type
