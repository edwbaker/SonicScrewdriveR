# Pad labels with interval

Takes labels from Google Speech API transcript and pads the time by a
specified number of seconds.

## Usage

``` r
labelPadding(t, pad = 0.5, max_t = NULL)
```

## Arguments

- t:

  Transcript from Google Speech API

- pad:

  Amount of time (in seconds) to add to start and end

- max_t:

  Optional. The duration of the file, so padding does not exceed length
  of file.

## Value

A modified Google Speech API transcript object

## Examples

``` r
if (FALSE) { # \dontrun{
labelPadding(t, pad=2, max_t=duration(wave))
} # }
```
