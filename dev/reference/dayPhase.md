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

  Duration of recording in seconds

- lat:

  Latitude of recording device

- lon:

  Longitude of recording device

- tz:

  Time-zone of recording device when recording was made

## Value

Data frame of day phases with absolute timestamps and relative times
within file

## Details

Each phase is named for the solar event that begins it, so that the
Dusk.Civil phase runs from sunset to the end of civil twilight, and the
phases returned account for the whole of the recording. A phase that
happens twice within one recording is numbered, as Day and Day1.

The phases of the day do not all happen everywhere. At 50 degrees north
in midsummer the sun never reaches astronomical twilight, so there is no
Night, and the Dusk.Astro phase runs from the end of nautical twilight
through to the next morning; in polar day and polar night a single phase
can last for months, and is named for the sun's altitude through it. A
phase therefore need not start or end on the day of the recording, and
the start and end given for it are the real ones, however far outside
the recording they fall.

The moon does not divide the day into phases and is reported alongside
them, as one row for each night of moonlight the recording falls in.

## Examples

``` r
dayPhase(time=as.POSIXct("2024-06-15 20:00:00", tz="UTC"), duration=40000)
#>                          Start                 End Start.relative End.relative
#> Day        2024-06-15 03:48:00 2024-06-15 20:01:06              0           66
#> Sunset     2024-06-15 20:01:06 2024-06-15 20:05:23             66          323
#> Dusk.Civil 2024-06-15 20:05:23 2024-06-15 20:50:04            323         3004
#> Dusk.Naut  2024-06-15 20:50:04 2024-06-15 21:55:45           3004         6945
#> Dusk.Astro 2024-06-15 21:55:45 2024-06-16 01:53:06           6945        21186
#> Dawn.Naut  2024-06-16 01:53:06 2024-06-16 02:58:57          21186        25137
#> Dawn.Civil 2024-06-16 02:58:57 2024-06-16 03:43:41          25137        27821
#> Sunrise    2024-06-16 03:43:41 2024-06-16 03:47:58          27821        28078
#> Day1       2024-06-16 03:47:58 2024-06-16 20:01:33          28078        40000
#> Moon       2024-06-15 13:06:13 2024-06-16 00:41:13              0        16873
```
