# Photon transport Monte Carlo simulation

This program models photon transport by using Monte Carlo method and ENDF nuclear physics libraries. The following reactions are taken into account

## Theoretical basis

### Scattering reactions

- coherent scattering by using rejection sampling with CDF formed from form factors
- incoherent scattering by using Kahn's method (only for photons with energy less than 1.5 MeV) and rejection sampling with CDF formed from scattering functions
- photoelectric effect
- pair production in electron field and nuclear field

The program is work in progress.

## References

Below is a list of documents which were used as a theoretical basis for this program.

Kahn, Herman, Applications of Monte Carlo. Santa Monica, CA: RAND Corporation, 1956.
Persliden J. A Monte Carlo program for photon transport using analogue sampling of scattering angle in coherent and incoherent scattering processes. Comput Programs Biomed. 1983 Aug-Oct;17(1-2):115-28. doi: 10.1016/0010-468x(83)90032-6. PMID: 6689289.
Carter, L L and Cashwell, E D. "Particle-transport simulation with the Monte Carlo method." , Jan. 1975. https://doi.org/10.2172/4167844
Brown, David A.. "ENDF-6 Formats Manual - Data Formats and Procedures for the Evaluated Nuclear Data Files ENDF/B-VI, ENDF/B-VII and ENDF/B-VIII." , Sep. 2023. https://doi.org/10.2172/2007538
