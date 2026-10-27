# Calculate the natural frequency

Calculates the natural frequency given the inductance, capacitance and
resistance. In the acoustic case the inductance is inertia or mass, the
capacitance is elasticity (bulk modulus) and resistance is composed of
air resistance and related quantities. All units are SI.

## Usage

``` r
naturalFrequency(L, C = 1e+05, R)
```

## Arguments

- L:

  Inductance

- C:

  Capacitance, by default the IUPAC standard pressure of 1e5 Pascals.

- R:

  Resistance

## Value

The natural frequency, in Hertz.

## Details

For isothermal compression, the bulk modulus is equal to the pressure.
The default value of C therefore is the IUPAC standard pressure, which
in SI units is 1e5 Pascals.

A system in which the damping term exceeds the stiffness term
(R^2/(4L^2) \> 1/(LC)) is overdamped: it returns to rest without
oscillating, and so has no real natural frequency. This raises an error
rather than returning NaN.

## Examples

``` r
naturalFrequency(L=20,R=0.01)
#> [1] 0.0001052711
naturalFrequency(L=20,C=1/4,R=0.5)
#> [1] 0.07114845
```
