# Convert time to seconds

Converts time measurements into seconds

## Usage

``` r
convert2seconds(T, input = "minutes", origin = "day", timezone = NULL)
```

## Arguments

- T:

  The time value to convert

- input:

  The unit of time to convert, allowed values are "minutes", "hours",
  "days", "years", "HHMM".

- origin:

  For POSIX whether to return relative to start of day ("day") or Unix
  epoch ("unix")

- timezone:

  The timezone to read a POSIX time in, which decides when its day
  starts. The default, NULL, uses the timezone the time itself carries,
  and UTC for a time that carries none (such as the output of
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html)).

## Value

The numeric value in seconds
