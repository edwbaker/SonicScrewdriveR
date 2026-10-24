# Get the wavelength from frequency and speed of sound

Calculates the wavelength of a sound wave given the frequency and speed
of sound in that medium.

## Usage

``` r
wavelength(frequency, speed = soundSpeed(medium = "air"), unit = "m")
```

## Arguments

- frequency:

  Frequency of the sound in Hertz

- speed:

  Speed of sound (defaults to the speed of sound in air)

- unit:

  Unit to return the wavelength in, either "m" or "cm"

## Value

Wavelength of the sound in the unit specified

## Examples

``` r
wl <- wavelength(1000)
wl <- wavelength(1000, speed=soundSpeed(medium="steel"))
wl <- wavelength(1000, unit="cm")
```
