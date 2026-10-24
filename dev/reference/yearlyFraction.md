# Calculate the fraction of a year given by a value

Given an object that can be coerced to POSIXlt, return the fraction of a
year represented by the object.

## Usage

``` r
yearlyFraction(
  t,
  year = 2022,
  input = "POSIX",
  unit = "radians",
  timezone = NULL
)
```

## Arguments

- t:

  Object to be converted to a fraction

- year:

  Year to calculate fractions of (allows for leap years)

- input:

  One of POSIXlt (default)

- unit:

  If set to radians outputs a position around a circle. If set to
  fraction outputs the raw fraction.

- timezone:

  The timezone to read the time in, which decides which day of the year
  it falls on. The default, NULL, uses the timezone the time itself
  carries, and UTC for a time that carries none (such as the output of
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html)).

## Value

The position of the date within the year, in radians or as a fraction of
a year.
