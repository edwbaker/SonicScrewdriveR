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
configuration files and wave files can be read into R using the
[`audiomothConfig()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothConfig.md)
and
[`audiomothWave()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothWave.md)
functions, respectively. The
[`audiomothConfig()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothConfig.md)
function takes a path to a configuration file and returns a data frame
of the configuration settings. The
[`audiomothWave()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothWave.md)
function takes a path to a wave file and returns a list of extracted
parameters.

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
a <- audioblast("data", "annomate", source="bio.acousti.ca", id=10754, output="Annotations")
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
