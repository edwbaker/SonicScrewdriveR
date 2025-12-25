# Tag a Wave or WaveMC object

This function takes a `Wave`/`WaveMC` object (or a list of such objects)
and returns a corresponding tagged version (`TaggedWave` or
`TaggedWaveMC`).

## Usage

``` r
tagWave(w, origin = "user")
```

## Arguments

- w:

  A `Wave` or `WaveMC` object (or list of such objects).

- origin:

  The origin of the object (default "user").

## Value

A `TaggedWave` or `TaggedWaveMC` object (or list of such objects).
