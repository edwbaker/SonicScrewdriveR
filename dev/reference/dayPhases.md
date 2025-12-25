# Phases of day

Wrapper for suncalc::getSunlightTimes that formats output for this
package.

## Usage

``` r
dayPhases(time = as.Date(Sys.time()), lat = 50.1, lon = 1.83, tz = "UTC")
```

## Arguments

- time:

  A time object representing the start time of a recording

- lat:

  Latitude of recording device

- lon:

  Longitude of recording device

- tz:

  Time-zone of recording device when recording was made
