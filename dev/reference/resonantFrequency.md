# Calculate the resonant frequency

Calculates the resonant frequency given the inductance and capacitance.
In the acoustic case the inductance is inertia or mass, the capacitance
is elasticity (bulk modulus) and resistance is composed of air
resistance and related quantities. All units are SI.

## Usage

``` r
resonantFrequency(L, C = "default")
```

## Arguments

- L:

  Inductance

- C:

  Capacitance, by default IUPAC standard pressure.

## Details

For isothermal compression, the bulk modulus is equal to the pressure.
The default value of C therefore is the IUPAC standard pressure.

## Examples

``` r
f <- resonantFrequency(L=1)
```
