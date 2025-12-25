# Calculate the natural frequency

Calculates the natural frequency given the inductance, capacitance and
resistance. In the acoustic case the inductance is inertia or mass, the
capacitance is elasticity (bulk modulus) and resistance is composed of
air resistance and related quantities. All units are SI.

## Usage

``` r
naturalFrequency(L, C = "default", R)
```

## Arguments

- L:

  Inductance

- C:

  Capacitance, by default IUPAC standard pressure.

- R:

  Resistance

## Details

For isothermal compression, the bulk modulus is equal to the pressure.
The default value of C therefore is the IUPAC standard pressure.

## Examples

``` r
naturalFrequency(L=20,R=0.5)
#> [1] 0.002950812
naturalFrequency(L=20,C=1/4,R=0.5)
#> [1] 0.07114845
```
