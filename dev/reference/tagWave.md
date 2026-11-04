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

## Details

A plain wave keeps the file it was read from, which
[`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md)
records, and the metadata of that file, which `audioMetadata<-`
attaches, as attributes. Tagging moves both into the `metadata` slot,
under the names `"audioFile"` and `"audioMetadata"`.
