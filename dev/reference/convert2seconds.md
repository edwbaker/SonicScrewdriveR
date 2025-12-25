# Convert time to seconds

Converts time measurements into seconds

## Usage

``` r
convert2seconds(T, input = "minutes", origin = "day")
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

## Value

The numeric value in seconds
