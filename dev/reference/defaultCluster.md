# Create Default Cluster for Windowing

Creates a default cluster using one less than the total cores available
on the system. Where the number of cores cannot be determined a cluster
of one is created. By default this uses forking, which is not be
available on Windows. Hence, the fork parameter has no effect on
Windows.

## Usage

``` r
defaultCluster(fork = TRUE, seed = NULL)
```

## Arguments

- fork:

  If TRUE uses forking to create the cluster (Unix like systems only)

- seed:

  An integer to seed the workers' random number streams with, or NULL
  (default) to take them from the session's own random number state.

## Value

A cluster object for parallel processing

## Details

The workers are given parallel random number streams, so that a windowed
analysis that uses randomness can be run again and give the same
numbers. Without them each worker seeds itself from its own entropy and
[`set.seed()`](https://rdrr.io/r/base/Random.html) in the session cannot
reach it, so the analysis gives different numbers on every run. Where
`seed` is NULL the streams are drawn from the session's own random
number state, so [`set.seed()`](https://rdrr.io/r/base/Random.html)
before the call fixes them, and that state and the session's generator
are left as they were found.

The streams are one per worker, so which worker computes a window
decides which numbers it draws: reproducing a result needs a cluster of
the same size as well as the same seed, and the same result cannot be
expected of
[`windowing()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/windowing.md)
without a cluster, which draws from the session's own stream.

## Examples

``` r
if (FALSE) { # \dontrun{
cl <- defaultCluster()
stopCluster(cl)
cl <- defaultCluster(FALSE)
stopCluster(cl)

# The same numbers on every run, from the session's seed or from the
# cluster's own.
set.seed(42)
cl <- defaultCluster()
stopCluster(cl)
cl <- defaultCluster(seed=42)
stopCluster(cl)
} # }
```
