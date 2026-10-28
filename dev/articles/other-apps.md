# Interacting with other tools

``` r

library(sonicscrewdriver)
```

## Introduction

The `sonicscrewdriver` package provides a number of functions for
interacting with other audio tools and software. This vignette provides
an overview of the functions available for interacting with other tools.

## Interfaces with other tools

### Audacity

Label files exported from [Audacity](https://www.audacityteam.org/) can
be read into R using the
[`readAudacityLabels()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudacityLabels.md)
function. This function takes a path to a label file and returns a list
of `Annotation` objects or a data frame with the start and end times of
each label and the label text. The
[`writeAudacityLabels()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/writeAudacityLabels.md)
function can be used to write a list of `Annotation` objects to an
Audacity label file.

### AudioBlast

The [audioBlast API](https://api.audioblast.org) can be accessed using
the `audioBlast()` function. Files can be downloaded from the API using
the `audioBlastDownload()` function.

### AudioMoth

[AudioMoth](https://www.openacousticdevices.info/audiomoth)
configuration files can be read into R using the
[`audiomothConfig()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothConfig.md)
function, which takes a path to a configuration file and returns a data
frame of the configuration settings.

The metadata an AudioMoth writes into a wave file is read by
[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md),
as its `audiomoth` source:

``` r

m <- readMetadata("20201103_112430.WAV")
m$sources$audiomoth$serial
m$sources$audiomoth$temperature

# The recording time, as a POSIXct in the zone the device was set to
m$fields$datetime
```

[`audiomothWave()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothWave.md)
did the same for a wave file alone and is deprecated in favour of this.

#### Triggered recordings

An AudioMoth recording on a trigger, such as an amplitude threshold,
writes a single file whose name ends in `T.WAV`. The periods that did
not trigger a recording are left out of it, each replaced by a short
encoded block recording how long it was. A conventional wave tool reads
such a file as the triggered recordings run together, which loses the
timing of the events in the recording.

[`audiomothExpand()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothExpand.md)
puts the omitted periods back as silence, so that the triggered
recordings sit where they were recorded. It returns a Wave object, or
writes an ordinary wave file if given a path to write to.

``` r

w <- audiomothExpand("20201103_112430T.WAV")

# Or expand every triggered recording on a card to a wave file
for (f in list.files(pattern="T\\.WAV$")) {
  audiomothExpand(f, sub("T\\.WAV$", ".WAV", f))
}
```

[`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md)
recognises a triggered recording and expands it as it reads it, so no
change is needed to code that already uses it. It is recognised by the
trigger AudioMoth records in the comment it writes into the header,
rather than by the `T` before the extension: a name is only a
convention, which an ordinary recording can meet and a triggered one can
be renamed out of.
[`windowing()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/windowing.md)
reads such a file the same way, so windows are scheduled over the
recording rather than over what is left of it on disk.

``` r

w <- readAudio("20201103_112430T.WAV", from=60, to=120, units="seconds")
```

[`audiomothCompress()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothCompress.md)
goes the other way, writing a Wave as a triggered file and leaving out
the segments whose peak amplitude falls below a threshold. Audio left
out is not recoverable, as it is not recoverable on the device. The
threshold is recorded in the file’s comment, as a device records it,
which is what lets the file be read back as a triggered recording.

``` r

audiomothCompress(w, "20201103_112430T.WAV", threshold=1000)
```

#### GUANO metadata

Current AudioMoth firmware writes
[GUANO](https://github.com/riggsd/guano-spec) metadata, a format used by
many bioacoustic recorders, which
[`guano()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guano.md)
reads from any wave file that carries it.

``` r

m <- guano("20201103_112430.WAV")
m[["Loc Position"]]
m[["Timestamp"]]
```

Fields the specification defines are converted to the types it gives
them, so a position is a pair of numbers and a timestamp is a `POSIXct`.
Any other field, which includes every namespaced field such as
`OAD|Battery Voltage`, is kept as written. Metadata can be edited as a
list and written back with
[`guanoWrite()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guanoWrite.md),
which replaces only the metadata and leaves the audio untouched.

``` r

m[["Species Manual ID"]] <- "Pipistrellus pipistrellus"
guanoWrite(m, "20201103_112430.WAV")
```

[`guanoCreate()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guanoCreate.md)
builds metadata from scratch, which
[`guanoWrite()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guanoWrite.md)
can add to a file that has none, and
[`audiomothCompress()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothCompress.md)
can write into a triggered file. Expanding a triggered file carries its
metadata over.

``` r

m <- guanoCreate(
  Make = "Open Acoustic Devices",
  Model = "AudioMoth",
  Samplerate = 48000,
  "Loc Position" = c(51.4964, -0.1224)
)
guanoWrite(m, "20201103_112430.WAV")
```

### BirdNET Analyzer

[BirdNET Analyzer](https://birdnet.cornell.edu) is a deep learning model
for the automatic detection of bird sounds. The `birdNetInstall()`
function can be used to install the `ssd_birdnet` environment required
to use the
[`birdNetAnalyse()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/birdNetAnalyse.md)
function with `SonicScrewdriver`. The
[`birdNetAnalyse()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/birdNetAnalyse.md)
function takes a list of sound files and analyses them using the
`BirdNET-Analyzer`. The function either returns a data frame with the
results of the analysis or a list of `Annotation` objects.

``` r

# Install the Python environment
pythonInstall()
```

``` r

# Analyse sound files using BirdNET-Analyzer
f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
birdNetAnalyse(f, lat=51.5, lon=0.1, date=as.Date("2021-01-01"))
```

### scikit-maad

[scikit-maad](https://scikit-maad.github.io/index.html) is an open
source Python package dedicated to the quantitative analysis of
environmental audio recordings. `sonicscrewdriver` makes use of the
`reticulate` package to create a Python environment and to interact with
scikit-maad. In `sonicscrewdriver` scikit-maad functions are prefixed
with `maad_`.

The functions provided accept standard Wave-like objects (i.e. `tuneR`
`Wave` or `WaveMC` objects, as well as their tagged equivalents from
this package).

``` r

# Install the Python environment
pythonInstall()
```

``` r

f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
w <- readWave(f)

maad_aci <- maad_acoustic_complexity_index(w)
```

## Exemplar applications

### Audio file with annotations from audioBlast to Audacity

The audio file of interest (id:
[10754](https://bio.acousti.ca/node/10754)) is a recording of the Mole
Cricket *Gryllotalpa vineae* from the
[BioAcoustica](https://bio.acousti.ca) repository. Recordings and
annotations from BioAcoustica are made available through the
[audioBlast](https://audioblast.org) API.

First we will download the audio file and annotations from audioBlast.

``` r

# Find the file on audioblast
data <- audioblast("data", "recordings", source="bio.acousti.ca", id=10754)

# Download the file to the mole_cricket directory
audioblastDownload(data, dir="mole_cricket/")
```

This has downloaded the audio file to the `mole_cricket` directory. It
has also saved a `metadata.csv` file that includes more information
about the audio file retrieved from audioBlast.

Next, we will download the annotations for this file from audioBlast.

``` r

# Fetch annotations from audioBlast as `Annotation` objects
a <- audioblast("data", "annomate", source="bio.acousti.ca", source_id=10754, output="Annotations")
```

This has downloaded the annotations from audioBlast and converted them
to a list of `Annotation` objects. Once annotation data are converted to
`Annotation` objects they can be easily manipulated into a number of
other useful formats.

In this case, we will convert the `Annotation` objects to an Audacity
label file.

``` r

# Convert the annotations to an Audacity label file
writeAudacityLabels(a, "mole_cricket/annotations.txt")
```

When we are finished, we can tidy our workspace by removing the
`mole_cricket` directory.

``` r

unlink("mole_cricket", recursive=TRUE)
```

## BirdNET Analyzer to Audacity label file

The
[`birdNetAnalyse()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/birdNetAnalyse.md)
function can be used to analyse sound files using the
`BirdNET-Analyzer`.

Get the output of the BirdNET Analyzer as a list of `Annotation`
objects.

``` r

# Analyse sound files using BirdNET-Analyzer
f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
annotations <- birdNetAnalyse(f, output="Annotation")
```

Convert the `Annotation` objects to an Audacity label file.

``` r

# Convert the annotations to an Audacity label file
writeAudacityLabels(annotations, "birdnet_annotations.txt")
```

Cleaning up the workspace.

``` r

unlink("birdnet_annotations.txt")
```
