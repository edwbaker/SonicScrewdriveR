# Calculate the speed of sound in a medium

Given sufficient parameters (i.e. wavelength and frequency, bulk modulus
and density) this function calculates the speed of sound.

## Usage

``` r
soundSpeed(
  medium = NULL,
  method = NULL,
  wl = NULL,
  f = NULL,
  bulkModulus = NULL,
  density = NULL,
  ...
)
```

## Arguments

- medium:

  Propagation medium (e.g. "air"), or "all" to return a list of all
  available media.

- method:

  Use a specific method to calculate the speed of sound (see Details).

- wl:

  Wavelength

- f:

  Frequency

- bulkModulus:

  Bulk modulus

- density:

  Density

- ...:

  Additional parameters passed to the method.

## Details

The speed of sound can be calculated using the following methods:

- **cramer** Uses the method described in Cramer (1993) . Additional
  parameters are:

  - temp Temperature

  - temp.unit Temperature unit

  - pressure Pressure

  - pressure.unit Pressure unit

  - RH Relative humidity

  - MoleFracCO2 Mole fraction of CO2

- **seewave** Delegates the calculation of the speed of sound in air to
  the package `seewave` (Sueur et al. 2008) . This calculation is .
  performed as \\\text{speed} = 331.4 + 0.6 \times \text{temp}\\.
  Additional parameters are:

  - temp Temperature

## References

Cramer O (1993). “The variation of the specific heat ratio and the speed
of sound in air with temperature, pressure, humidity, and CO2
concentration.” *The Journal of the Acoustical Society of America*,
**93**(5), 2510–2516. ISSN 0001-4966,
[doi:10.1121/1.405827](https://doi.org/10.1121/1.405827) .  
  
Sueur J, Aubin T, Simonis C (2008). “Seewave, a free modular tool for
sound analysis and synthesis.” *Bioacoustics*, **18**(2), 213–226.

## Examples

``` r
soundSpeed(medium="air")
#> [1] 343
soundSpeed(medium="sea water")
#> [1] 1500

soundSpeed(method="cramer", temp=14, pressure=3, RH=10)
#> [1] 342.682
soundSpeed(method="cramer", temp=14, temp.unit="C", pressure=3, pressure.unit="kPa", RH=10)
#> [1] 342.682

t <- 1:30
s <- lapply(t, \(x){soundSpeed(method="cramer", temp=x)})
```
