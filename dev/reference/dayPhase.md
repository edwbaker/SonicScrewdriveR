# Phase of day

Given a start time and (optionally) a duration returns the phase of day
at a given location. This is primarily used to calculate phase of day
information for soundscape recording projects.

## Usage

``` r
dayPhase(
  time = Sys.time(),
  duration = 40000,
  lat = 50.1,
  lon = 1.83,
  tz = "UTC"
)
```

## Arguments

- time:

  A time object representing the start time of a recording

- duration:

  Duration of recording

- lat:

  Latitude of recording device

- lon:

  Longitude of recording device

- tz:

  Time-zone of recording device when recording was made

## Value

Data frame of day phases with absolute timestamps and relative times
within file
