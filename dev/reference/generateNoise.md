# Add noise to a Wave like object

Adding noise to a Wave like object allows for testing of the robustness
of automated identification algorithms to noise.

## Usage

``` r
generateNoise(
  wave,
  noise = c("white"),
  noise.add = FALSE,
  noise.ratio = 0.5,
  noise.ref = "file",
  output = "list"
)
```

## Arguments

- wave:

  Object to add noise to (`Wave`, `WaveMC`, or Tagged versions), or a
  list of such objects.

- noise:

  Vector of noise to add (unif, gaussian, white, pink, power, red)

- noise.add:

  If TRUE all noise sources are added to wave. If FALSE separate outputs
  are created for each noise source.

- noise.ratio:

  Ratio of maximum noise amplitude to the maximum amplitude in wave.

- noise.ref:

  Reference maximum for noise.ratio. If "max" then the maximum
  amplitude, if "file" then the maximum amplitude of wave.

- output:

  TODO: Is this implemented?

## Value

A list of Wave objects with the required noise added.
