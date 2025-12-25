# Short term energy

Computes the short term energy of a Wave.

## Usage

``` r
ste(wave, method = "dietrich2004", ...)
```

## Arguments

- wave:

  A Wave object

- method:

  Which method used to calculate the short term energy, by default
  "dietrich2004" to use (Dietrich et al. 2004) .

- ...:

  Other arguments to pass to ste method.

## Value

A vector of short term energy values

## References

Dietrich C, Palm G, Riede K, Schwenker F (2004). “Classification of
bioacoustic time series based on the combination of global and local
decisions.” *Pattern Recognition*, **37**(12), 2293–2305.

## Examples

``` r
if (FALSE) { # \dontrun{
ste(sheep, method="dietrich2004")
} # }
```
