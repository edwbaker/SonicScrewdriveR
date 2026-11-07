# Write an AOEF file

Writes a list of `Annotation` objects to a file in the Acoustic Objects
Exchange Format (AOEF), as an annotation set or as an annotation
project, which the soundevent Python package and the Whombat annotation
tool (Balvanera et al. 2023) read.

## Usage

``` r
writeAOEF(
  annotations,
  file,
  audio.dir = NULL,
  collection = "annotation_set",
  name = NULL,
  description = NULL
)
```

## Arguments

- annotations:

  A list of `Annotation` objects, or a single one.

- file:

  Path to the AOEF file to write.

- audio.dir:

  The directory the paths of the recordings are to be written relative
  to, which every annotated file must be in. Where it is `NULL` the
  paths are written as the annotations have them.

- collection:

  One of "annotation_set" or "annotation_project". Whombat imports only
  annotation projects, which are annotation sets with a task for each
  clip, and whose tags, the ones offered to those annotating it, are
  every tag written.

- name, description:

  A name and a description of the collection. An annotation project is
  named after the file where no name is given.

## Value

Invisibly, the path of the file written.

## Details

Each annotation is written as a sound event, tagged with its `type` as
the key and its `value` as the value. An annotation with a value and no
type is tagged with the key "label", and one with neither is written
without a tag. Its shape is a stretch of time where it has no frequency
limits, a single moment where it also starts where it ends, and
otherwise a box of time and frequency, or a single point where the box
has no size. An end of `Inf` is the end of the recording, and a high
frequency of `Inf` above a low one is half the sample rate.

Annotations read by
[`readAOEF()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAOEF.md)
are written back as they were read: those of one sound event with
several tags as one sound event again, a line or polygon as that shape
where its range has not been changed, the tags of a clip on that clip,
and each sound event in the clip it was in, under the identifiers it
had. A sound event moved off its clip altogether is put in a clip
spanning its recording, and one of several tags given a range of its own
becomes a sound event of its own. Whombat imports the annotations of a
project only for recordings it already holds under the same identifiers,
which those annotations keep. The `identifiedBy` of an annotation is
written as who made it (`created_by`) and its `dateIdentified` as when
(`created_on`), where that is a single date and time of day: AOEF holds
no date without a time, and one is not made up, nor a range. `notes` and
`features` in the `metadata` of an annotation are written; nothing else
of it is, so predictions are written as annotations without their
scores.

soundevent reads only annotations in a clip, so every other annotation
is put in a clip spanning the whole of its recording. AOEF also needs
the duration, channels and sample rate of each recording, which are read
from the file of the annotation, or taken from what
[`readAOEF()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAOEF.md)
kept of it. Reading a file other than a wave file needs the av package.

Characters outside ASCII are written as escapes, so that the file reads
the same whatever encoding a reader takes it to be in.

## References

Balvanera SM, Mac Aodha O, Weldy MJ, Pringle H, Browning E, Jones KE
(2023). “Whombat: An open-source annotation tool for machine learning
development in bioacoustics.”
[doi:10.48550/arXiv.2308.12688](https://doi.org/10.48550/arXiv.2308.12688)
. 2308.12688, <https://arxiv.org/abs/2308.12688>.

## See also

[`readAOEF()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAOEF.md)
