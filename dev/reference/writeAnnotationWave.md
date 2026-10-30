# Save a time region defined by an Annotation object as a Wave file

The region is cut from the wave by
[`cutws()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/cutws.md),
so a stereo or multichannel wave keeps all of its channels and its bit
depth.

## Usage

``` r
writeAnnotationWave(annotation, wave = NULL, dir = ".")
```

## Arguments

- annotation:

  An Annotation object, or a list of them.

- wave:

  (Optional) A Wave object, if not given will load the wave file from
  Annotation filename.

- dir:

  The directory to write the files to.

## Value

Invisibly, the path of each file written, or `NA` for an annotation
whose region holds none of the wave.

## Details

Each region is written to `dir`, in a file named from the annotated
file, the start and end of the annotation and, where it has one, its
value, as `AUDIOMOTH_0-3_Turdus_merula.wav`. Characters of the value
other than letters, digits, dots, dashes and underscores are written as
underscores. Two annotations of the same region, such as two species
found in the same window by
[`birdNetAnalyse()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/birdNetAnalyse.md),
are written to two files.
