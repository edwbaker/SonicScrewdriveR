# Convert time to seconds

Converts time measurements into seconds

## Usage

``` r
convert2seconds(
  t,
  input.unit = "minutes",
  origin = "day",
  timezone = NULL,
  T,
  input
)
```

## Arguments

- t:

  The time value to convert

- input.unit:

  The unit of time to convert, one of "seconds", "minutes", "hours",
  "days", "years", "HHMM", "POSIX".

- origin:

  For POSIX whether to return relative to start of day ("day") or Unix
  epoch ("unix")

- timezone:

  The timezone to read a POSIX time in, which decides when its day
  starts. The default, NULL, uses the timezone the time itself carries,
  and UTC for a time that carries none (such as the output of
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html)).

- T:

  Deprecated, use t instead.

- input:

  Deprecated, use input.unit instead.

## Value

The numeric value in seconds
