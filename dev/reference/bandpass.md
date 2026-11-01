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

The frequencies the filter passes keep the level they had. ffilter
returns its output at one part in `wl` of the input's level, 1024 by
default, unless asked to rescale it, so the output is scaled back by
`wl`.

## Examples

``` r
if (FALSE) { # \dontrun{
nwave <- noise("white", duration=44100, samp.rate=44100)

fwave <- filterWave(nwave, bandpass(from=1000, to=2000))
nwave |> filterWave(bandpass(from=1000, to=2000)) -> fwave
} # }
```
