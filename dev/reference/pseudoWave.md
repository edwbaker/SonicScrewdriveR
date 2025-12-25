# Create a PseudoWave object

This function is used to create a PseudoWave object that can be used to
generate a Wave object when operated on.

## Usage

``` r
pseudoWave(
  type = NA_character_,
  subtype = NA_character_,
  scale = 1,
  offset = 0,
  seed = 1,
  params = list()
)
```

## Arguments

- type:

  Type of PseudoWave (e.g. "noise", "sine")

- subtype:

  Subtype of PseudoWave (e.g. "white" if type is "noise")

- scale:

  The Wave channels are multiplied by this value

- offset:

  This value is added to the Wave channels

- seed:

  Random seed for reproducible output. NA for no

- params:

  List of additional parameters to pass to generating function

## Value

A PseudoWave object.

## Examples

``` r
pw <- pseudoWave("noise", "white")

pw <- pseudoWave("sine", params=list("f0"=440))

pw <- pseudoWave("file", "myfile.wav")
#> Error in pseudoWave("file", "myfile.wav"): File does not exist
```
