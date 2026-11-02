# Simple bandpass filter

Creates a band pass WaveFilter between values specified to a Wave
object.

## Usage

``` r
bandpass(from, to, ...)
```

## Arguments

- from:

  Bottom of bandpass frequency (Hz).

- to:

  Top of bandpass frequency (Hz).

- ...:

  Further arguments to pass to ffilter.

## Value

A WaveFilter object.

## Details

This is a simple wrapper function to the seewave ffilter function
allowing its use with filterw and pipes.

ffilter returns its output at one part in `wl` of the input's level,
1024 by default, unless asked to rescale it.
[`filterWave()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/filterWave.md)
normalises the result to the peak of the wave it was given, so it comes
back at the level of the input whatever window it was filtered with.

## Examples

``` r
if (FALSE) { # \dontrun{
nwave <- noise("white", duration=44100, samp.rate=44100)

fwave <- filterWave(nwave, bandpass(from=1000, to=2000))
nwave |> filterWave(bandpass(from=1000, to=2000)) -> fwave
} # }
```
