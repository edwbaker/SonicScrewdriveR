# Rainfall detection

Detects rainfall in a Wave. The "bedoya2017" method follows (Bedoya et
al. 2017) : the mean power spectral density of the 600-1200 Hz band,
where rain falling on vegetation is loudest, together with the ratio of
that mean to its standard deviation across the band, both of which must
clear a threshold. One band is what the paper specifies; the two bands
of the hardRain package are that package's own addition rather than part
of the method cited here.

## Usage

``` r
rainfallDetection(wave, method = "bedoya2017", ...)
```

## Arguments

- wave:

  A Wave object to detect rainfall in. Stereo Wave and WaveMC objects
  are passed to allChannels(), which returns one value per channel.

- method:

  Which rainfall detection method to use ("bedoya2017")

- ...:

  Other arguments to pass to rain detection function

## Value

Numeric value from the rainfall detection algorithm chosen, or zero
where the recording is not rainfall. For "bedoya2017" the value is the
mean power spectral density of the 600-1200 Hz band in power per Hz,
relative to the full scale of the format the wave is held in, so it is
the same number whatever that bit depth is and is comparable between
recordings. Its `Tmean` threshold is in those units. For multi-channel
input, a list of such values.

## Details

Two things here are not the paper's. `Tsnr` defaults to 3.5 where the
paper reports 1.8, and the ratio is taken across the band's bins of a
spectrum already averaged over the recording, which makes it rise with
the recording's length: the same noise gives 4.0 over 0.05 s and 49 over
60 s. On labelled recordings that measure still separates rain (11 to
27) from everything else (1.4 to 5.8) far more cleanly than the
alternatives, which is why it is kept, but `Tsnr` is a figure for a
fixed clip length rather than an absolute one.

`Tmean` is in power per Hz relative to full scale and defaults to the
paper's 0.9e-6, which is a calibration rather than a constant: the level
of a band depends on recorder gain, and across ten labelled rain
recordings from one site it ran from 2.1e-08 to 1.7e-05. Estimate it
from recordings of your own recorder, as the hardRain package and the
authors of the method both do.

The ratio needs at least two frequency bins in the band, and a bin of
the 512-sample windows the method uses is the sample rate over 512 wide.
At 204.8 kHz and above, which takes in the 250 and 384 kHz of bat
detectors, the band holds one bin at most, and a wave sampled at 1.2 kHz
or less does not reach the band at all. Where the band holds fewer than
two bins, the method stops with an error saying why, which in
[`runPipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/runPipeline.md)
fails the file. At 48 kHz, for instance, the band holds six.

The hardRain package can also be accessed via this wrapper.

## References

Bedoya C, Isaza C, Daza JM, López JD (2017). “Automatic identification
of rainfall in acoustic recordings.” *Ecological Indicators*, **75**,
95–100.
[doi:10.1016/j.ecolind.2016.12.018](https://doi.org/10.1016/j.ecolind.2016.12.018)
.

## Examples

``` r
if (FALSE) { # \dontrun{
rainfallDetection(sheep, method="bedoya2017")
} # }
```
