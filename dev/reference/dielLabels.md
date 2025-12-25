# Generate labels for a diel plot

Generates labels for a dielPlot() in 12- or 24-hour format. Labels are
generated at three hourly intervals.

## Usage

``` r
dielLabels(format = "clock24")
```

## Arguments

- format:

  One of clock24 (default) or clock12

## Examples

``` r
dielLabels()
#> [1] "0000" "0300" "0600" "0900" "1200" "1500" "1800" "2100"
dielLabels("clock12")
#> [1] "0000"      "0300 AM"   "0600 AM"   "0900 AM"   "1200 NOON" "0300 PM"  
#> [7] "0600 PM"   "0900 PM"  
```
