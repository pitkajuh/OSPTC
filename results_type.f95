module results_type
  use coordinate_type

  type :: dose_rate
     type(coordinate) :: coord
     real(kind(1.d0)) :: energy
  end type dose_rate

  type :: results
  end type results


end module results_type
