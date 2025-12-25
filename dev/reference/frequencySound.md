# Get the frequency from wavelength and speed of sound

Calculates the frequency of a sound wave given the wavelength and speed
of sound in that medium.

## Usage

``` r
frequencySound(wl, s = soundSpeed(medium = "air"))
```

## Arguments

- wl:

  Wavelength

- s:

  Speed of sound (defaults to the speed of sound in air)

## Value

Frequency of the sound in Hertz

## Examples

``` r
f <- frequencySound(wl=100, s=343)
```
