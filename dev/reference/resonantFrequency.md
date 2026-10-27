# Calculate the resonant frequency

Calculates the resonant frequency given the inductance and capacitance.
In the acoustic case the inductance is inertia or mass, the capacitance
is elasticity (bulk modulus) and resistance is composed of air
resistance and related quantities. All units are SI.

## Usage

``` r
resonantFrequency(L, C = 1e+05)
```

## Arguments

- L:

  Inductance

- C:

  Capacitance, by default the IUPAC standard pressure of 1e5 Pascals.

## Value

The resonant frequency, in Hertz.

## Details

For isothermal compression, the bulk modulus is equal to the pressure.
The default value of C therefore is the IUPAC standard pressure, which
in SI units is 1e5 Pascals.

## Examples

``` r
f <- resonantFrequency(L=1)
```
