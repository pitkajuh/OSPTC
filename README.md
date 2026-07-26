# Photon transport Monte Carlo simulation

This program models photon transport by using Monte Carlo method and ENDF nuclear physics libraries. The following reactions are taken into account

- coherent scattering by using rejection sampling with CDF formed from form factors
- incoherent scattering by using Kahn's method (only for photons with energy less than 1.5 MeV) and rejection sampling with CDF formed from scattering functions
- photoelectric effect
- pair production in electron field and nuclear field

The program is work in progress.