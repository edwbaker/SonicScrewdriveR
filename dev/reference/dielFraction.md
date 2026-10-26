# Calculate the fraction of a day given by a value

Given an object that can be coerced to POSIXlt or is in a supported
string format, return the fraction of a day represented by the object.

## Usage

``` r
dielFraction(
  t,
  input.unit = "POSIX",
  output.unit = "radians",
  timezone = NULL,
  input,
  unit
)
```

## Arguments

- t:

  Object to be converted to a fraction

- input.unit:

  The unit the time is given in, one of "seconds", "minutes", "hours",
  "days", "years", "HHMM", "POSIX".

- output.unit:

  If set to radians outputs a position around a circle. If set to
  fraction outputs the raw fraction.

- timezone:

  The timezone to read the time in, which decides where in the day it
  falls. The default, NULL, uses the timezone the time itself carries,
  and UTC for a time that carries none (such as the output of
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html)).

- input:

  Deprecated, use input.unit instead.

- unit:

  Deprecated, use output.unit instead.

## Value

The position of the time within the day, in radians or as a fraction of
a day.
