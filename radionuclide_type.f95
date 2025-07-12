module radionuclide_type
  use random
  implicit none

  type, abstract :: radionuclide
     real :: activity, half_life
   contains
     procedure(create_pdf), deferred :: pdf
  end type radionuclide

  abstract interface
     function create_pdf(this) result(pdf1)
       import radionuclide
       class(radionuclide), intent(inout) :: this
       real :: pdf1
     end function create_pdf
  end interface

  type, extends(radionuclide) :: co_60
   contains
     procedure, pass :: pdf => pdf_co_60
  end type co_60

contains

  function pdf_co_60(this) result(pdf1)
    class(co_60), intent(inout) :: this
    real :: pdf1
    if(std_uniform_distribution()<=0.85) pdf1=1173.3
    pdf1=1332.3
  end function pdf_co_60



end module radionuclide_type
