# Percentile clipping for false-colour index spectrograms

Returns a function suitable for the `scale` argument of
[`fcis`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/fcis.md),
which limits an index to a range of percentiles before it is mapped to a
colour channel.

## Usage

``` r
fcisClip(probs = c(0.02, 0.98), log = FALSE)
```

## Arguments

- probs:

  The lower and upper percentiles to clip to.

- log:

  If TRUE, take base 10 logarithms first. Values of zero or less have no
  logarithm and are treated as the lower limit.

## Value

A function of one argument, returning the clipped values.

## Details

Mapping an index linearly between its smallest and largest value gives
most of the colour range to a handful of extreme windows. Indices that
span several orders of magnitude, such as power, are worse still, and
are better mapped after taking logarithms.

## Examples

``` r
if (FALSE) { # \dontrun{
#Power spans many orders of magnitude, so map it on a log scale.
plot(fcis(wave, window.length=44100, scale=list(fcisClip(log=TRUE), fcisClip(), fcisClip())))
} # }
```
