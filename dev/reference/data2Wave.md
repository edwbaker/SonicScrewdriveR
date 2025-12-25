# Convert data into a Wave object

Make a sequence of data into a normalised Wave object.

## Usage

``` r
data2Wave(
  left,
  samp.rate = 44100,
  bit = 16,
  unit = NULL,
  remove.offset = TRUE,
  normalise = TRUE
)
```

## Arguments

- left:

  Data for mono audio channel

- samp.rate:

  Sampling rate for Wave object

- bit:

  Bit depth of Wave object

- unit:

  See tuneR::normalize. If NULL this is handled automatically.

- remove.offset:

  If TRUE any DC offset is removed

- normalise:

  IF TRUE the output Wave is normalised to -1:1

## Value

A mono Wave object.

## Examples

``` r
pattern <- seq(from=-1, to=1, length.out=100)
data <- rep.int(pattern, 100)
w <- data2Wave(data)
```
