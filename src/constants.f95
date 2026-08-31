module constants
  public :: pi, h, c, hc, meter_in_angstroms, atomic_mass_unit, electron_mass

  real(kind(1.d0)) :: pi=3.141592653589793_8
  real(kind(1.d0)) :: pi2=3.141592653589793_8
  real(kind(1.d0)) :: h=4.135667696E-15_8 ! eVs
  real(kind(1.d0)) :: c=299792458.0_8 ! m/s
  real(kind(1.d0)) :: hc=4.135667696E-15_8*299792458.0_8 ! eV/m
  real(kind(1.d0)) :: hcA=4.135667696E-15_8*299792458.0_8*10_8**10_8 ! eV/A
  real(kind(1.d0)) :: meter_in_angstroms=10_8**10_8 ! A/m
  real(kind(1.d0)) :: atomic_mass_unit=931.494013_8*10_8**(6_8) ! eV/c^2
  real(kind(1.d0)) :: electron_mass=5.11E5_8 ! eV/c^2
  real(kind(1.d0)) :: energy_threshold_pair_nuc=2*5.11E5_8 ! eV/c^2
  real(kind(1.d0)) :: energy_threshold_pair_elec=4*5.11E5_8 ! eV/c^2
  real(kind(1.d0)) :: elementary_charge=1.602176462E-19_8
end module constants
