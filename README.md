# Photon transport Monte Carlo simulation

This program models photon transport by using Monte Carlo method and ENDF nuclear physics libraries.

## Theoretical basis

### Scattering reactions

ENDF nuclear physics libraries are parsed for coherent scattering form factors (27502) and incoherent scattering functions (27504). Cumulative distribution functions are formed for both reactions from the parsed data. During the runtime of the simulation, values from the cumulative distribution are sampled and scattering angles are calculated by using rejection sampling method. Kahn's method (only for photons with energy less than 1.5 MeV) is used for sampling incoherent scattering angles, which are then accepted or rejected by rejection sampling.

### Pair production

Pair production in nuclear field (23517) and electron field (23516) are possible reactions when the photon energy is >1.022 MeV >2.044 MeV because ENDF nuclear physics libraries shows the cross section to be exactly zero at these energies. Pair production in electron field is not possible in this program because the energy required for the reaction to happen is larger than the energy limit of the program. Electrons and positrons generated during are not taken into account, instead it assumed that the positron reacts with an electron immediately at the same location where the incident photon hit, forming two new 511 keV photons.

### Photoelectric effect

Electrons ejected during photoelectric effect reaction (23534...) are not taken into account. Incident photon loses some of its energy during the collision. Is assumed that the incident photon continues to its original direction with smaller energy. If the ionization energy is larger than the energy of the incident photon, the photon will be absorbed and disappears.

### Theoretical limits

The program does not take into account the following

- electrons are completely ignored
- energies above 1.5 MeV are not supported
- probably something else but not listed here

## References

Below is a list of documents which were used as a theoretical basis for this program.

1. Kahn, Herman, Applications of Monte Carlo. Santa Monica, CA: RAND Corporation, 1956.
2. Persliden J. A Monte Carlo program for photon transport using analogue sampling of scattering angle in coherent and incoherent scattering processes. Comput Programs Biomed. 1983 Aug-Oct;17(1-2):115-28. doi: 10.1016/0010-468x(83)90032-6. PMID: 6689289.
3. Carter, L L and Cashwell, E D. "Particle-transport simulation with the Monte Carlo method." , Jan. 1975. https://doi.org/10.2172/4167844
4. Brown, David A.. "ENDF-6 Formats Manual - Data Formats and Procedures for the Evaluated Nuclear Data Files ENDF/B-VI, ENDF/B-VII and ENDF/B-VIII." , Sep. 2023. https://doi.org/10.2172/2007538


# Build instructions

Install the following packages

``` bash
sudo apt-get install gfortran cmake
```

build

``` bash
mkdir build
cd build/
cmake -DCMAKE_BUILD_TYPE=RELEASE ../
make

```

# Running the program

Download photonuclear ENDF libraries and extract them

``` bash
wget https://www.nndc.bnl.gov/endf-releases/releases/B-VIII.1/gammas/gammas-version.VIII.1.tar.gz
mkdir cross-sections
tar -zxvf gammas-version.VIII.1.tar.gz -C cross-sections --strip-components=1

```

Run the simulation

``` bash
./osptc
```

Note that at the moment the only way to change the geometry is to edit the source code.

## Limits
### Materials/elements
At the moment only the following elements are supported

- oxygen
- hydrogen
- nitrogen
- titanium
- sodium
- iodine
- iron/steel

Materials with multiple elements are not supported at the moment.

### Geometries

At the moment only the following geometries are supported

- 3D box
- truncated cylinder along z-axis

# Planned features

- more geometries
- more materials
- materials with multiple elements
- read geometries and other hyperparameters from text files
- save the results to a file