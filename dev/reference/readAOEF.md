# Read an AOEF file

Reads the annotations held in a file in the Acoustic Objects Exchange
Format (AOEF), the JSON format of the soundevent Python package, in
which the Whombat annotation tool (Balvanera et al. 2023) exports its
projects. The predictions of a model run are read in the same way.

## Usage

``` r
readAOEF(file, audio.dir = NULL, output = "Annotation")
```

## Arguments

- file:

  Path to the AOEF file.

- audio.dir:

  The directory the paths of the recordings in the file are relative to,
  as soundevent's `audio_dir` is. Where it is `NULL` the paths are given
  as the file has them.

- output:

  One of "Annotation" or "data.frame".

## Value

A list of Annotation objects, or a data frame if output is "data.frame".

## Details

AOEF gives each sound event a shape: a stretch of time, a box of time
and frequency, a single moment, a single point, or lines and polygons
drawn on the spectrogram. Each is read as the smallest range of time and
frequency that holds it, which for a stretch of time or a single moment
is every frequency.

A sound event can carry several tags, each a key and a value such as
`dwc:scientificName` and `Turdus merula`, while an `Annotation` has one
`type` and one `value`. So a sound event is read as one annotation for
each of its tags, with the key as its `type`, or as one annotation with
no type and no value where it has no tags. The tags of a clip, a part of
a recording annotated as a whole, are read in the same way as
annotations spanning the clip. A clip with no tags and no sound events,
annotated as holding nothing, is read as one annotation with no type and
no value, so that the absence is kept.

Who made each annotation (`created_by`) is its `identifiedBy`, by the
person's name, and when (`created_on`) its `dateIdentified`. The
`metadata` of each annotation holds its `notes`, the `features` measured
of it, and for a prediction its `score` and the `tag_score` of its tag.
Under `aoef` it also holds the identifiers and the exact shape that
[`writeAOEF()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/writeAOEF.md)
needs to write it back as it was, and what the file said of its
recording. Sequences of sound events are not read.

soundevent on Windows writes in the encoding of the locale rather than
in UTF-8, so a file that is not UTF-8 is read as Windows-1252.

## References

Balvanera SM, Mac Aodha O, Weldy MJ, Pringle H, Browning E, Jones KE
(2023). “Whombat: An open-source annotation tool for machine learning
development in bioacoustics.”
[doi:10.48550/arXiv.2308.12688](https://doi.org/10.48550/arXiv.2308.12688)
. 2308.12688, <https://arxiv.org/abs/2308.12688>.

## See also

[`writeAOEF()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/writeAOEF.md)
