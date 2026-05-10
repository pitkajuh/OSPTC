module constants
  public :: pi, h, c, hc, meter_in_angstroms, atomic_mass_unit, electron_mass

  real(kind(1.d0)) :: pi=3.141592653589793_8
  real(kind(1.d0)) :: h=4.135667696E-15_8 ! eVs
  real(kind(1.d0)) :: c=299792458.0_8 ! m/s
  real(kind(1.d0)) :: hc=4.135667696E-15_8*299792458.0_8 ! eV/m
  real(kind(1.d0)) :: meter_in_angstroms=10_8**10_8 ! A/m
  real(kind(1.d0)) :: atomic_mass_unit=931.494013_8*10_8**(6_8) ! eV/c^2
  real(kind(1.d0)) :: electron_mass=5.485799110_8*10_8**(-4_8)*931.494013_8*10_8**(6_8) ! eV/c^2
  real(kind(1.d0)) :: Emax=2.5E6_8 ! maximum problem energy in eV
end module constants
