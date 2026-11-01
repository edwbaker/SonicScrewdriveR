# Calculate the duty cycle of a wave

Proportion of a wave with signal above the limit

## Usage

``` r
dutyCycle(wave, limit = 0.1, output = "unit", normalise = FALSE)
```

## Arguments

- wave:

  A Wave object. Stereo Wave and WaveMC objects are passed to
  allChannels(), which returns a list with one duty cycle per channel.

- limit:

  Threshold above which to consider the signal, as a proportion of the
  largest excursion from silence the wave's format can hold: the default
  of 0.1 is a tenth of full scale, whether the wave is floating point or
  integer, signed or unsigned. The threshold does not depend on the
  wave's own peak, so a loud transient does not change the duty cycle
  measured around it and the same sound recorded twice gives the same
  figure. In earlier releases this was a proportion of the loudest
  sample in the wave, which a value given explicitly still means only
  where that sample is at full scale.

- output:

  If "unit" the duty cycle will be in the range 0-1. For a percentage
  use "percent".

- normalise:

  If TRUE the Wave is normalised using tuneR before the threshold is
  applied. Normalising raises the wave's loudest sample to full scale,
  which makes the threshold depend on that sample again, and normalize()
  from tuneR removes the offset as well, so silence no longer sits at
  the centre of the format. The default is FALSE and was TRUE in earlier
  releases.

## Value

A numerical value for the duty cycle between 0 and 1 (or 0 and 100% if
percentage output). For multi-channel input, a list of such values.

## Examples

``` r
wave <- tuneR::sine(2000)
dc <- dutyCycle(wave)
pc <- dutyCycle(wave, output="percent")
```
