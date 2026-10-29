# Make a range to filter audioBlast on

Filters on a range, such as the duration of a recording or its latitude,
can be given a range made by this function. Both ends of the range are
included in it. Leaving out one end gives a range open at that end.

## Usage

``` r
ab_range(min = NULL, max = NULL)
```

## Arguments

- min:

  Lowest value of the range, or `NULL` for no lower limit.

- max:

  Highest value of the range, or `NULL` for no upper limit.

## Value

A range, to be given as the value of a filter to
[`audioblast()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblast.md).

## Examples

``` r
ab_range(10, 20)
#> <audioBlast range> 10:20 
ab_range(min=10)
#> <audioBlast range> >=10 
ab_range(max=20)
#> <audioBlast range> <=20 
if (FALSE) { # \dontrun{
audioblast("data", "recordings", duration=ab_range(10, 20))
} # }
```
