# Create Default Cluster for Windowing

Creates a default cluster using one less than the total cores available
on the system. By default this uses forking, which is not be available
on Windows. Hence, the fork parameter has no effect on Windows.

## Usage

``` r
defaultCluster(fork = TRUE)
```

## Arguments

- fork:

  If TRUE uses forking to create the cluster (Unix like systems only)

## Value

A cluster object for parallel processing

## Examples

``` r
if (FALSE) { # \dontrun{
cl <- defaultCluster()
stopCluster(cl)
cl <- defaultCluster(FALSE)
stopCluster(cl)
} # }
```
