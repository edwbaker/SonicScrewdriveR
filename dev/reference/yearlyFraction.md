# Calculate the fraction of a year given by a value

Given an object that can be coerced to POSIXlt, return the fraction of a
year represented by the object.

## Usage

``` r
yearlyFraction(t, year = 2022, input = "POSIX", unit = "radians")
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
