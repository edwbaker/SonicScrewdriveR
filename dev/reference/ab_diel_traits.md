# Convert text times of day in audioblast traits to numeric values

This function takes a traits dataset retrieved from audioblast and
converts values such as "day" into a numeric time of day based on the
date and location.

## Usage

``` r
ab_diel_traits(traits, date, lat, lon, overwrite = FALSE)
```

## Arguments

- traits:

  Traits dataset retrieved using audioblast().

- date:

  The date used for conversion for time.

- lat:

  Latitude of location.

- lon:

  Longitude of location.

- overwrite:

  If TRUE then the function will overwrite any existing min/max.
