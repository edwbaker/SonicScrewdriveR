# Calculate the fraction of a day given by a value

Given an object that can be coerced to POSIXlt or is in a supported
string format, return the fraction of a day represented by the object.

## Usage

``` r
dielFraction(t, input = "POSIX", unit = "radians")
```

## Arguments

- t:

  Object to be converted to a fraction

- input:

  One of POSIX (default) or HHMM

- unit:

  If set to radians outputs a position around a circle. If set to
  fraction outputs the raw fraction.
