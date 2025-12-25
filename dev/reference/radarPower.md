# The radar equation

Calculates the power returned from an echolocation pulse

## Usage

``` r
radarPower(P_t, r, area, G_t = 1, G_r = 1, wl = 1)
```

## Arguments

- P_t:

  Power transmitted (from sender)

- r:

  Range of the target

- area:

  Effective cross-sectional area of the target

- G_t:

  Transmitter gain

- G_r:

  Receiver gain

- wl:

  Wavelength (use only with G_r and G_t)

## Value

The received power

## Examples

``` r
radarPower(12, 20, 0.05)
#> [1] 1.889738e-09
radarPower(12, 20, 0.05, G_t=1.2, G_r=1.5, wl=0.045)
#> [1] 6.888096e-12
```
