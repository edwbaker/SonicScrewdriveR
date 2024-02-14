[![CRAN Version](https://www.r-pkg.org/badges/version/sonicscrewdriver)](https://cran.r-project.org/package=sonicscrewdriver) [![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/sonicscrewdriver)]() [![codecov](https://codecov.io/gh/edwbaker/SonicScrewdriveR/graph/badge.svg?token=pQq9E428KB)](https://codecov.io/gh/edwbaker/SonicScrewdriveR) [![R-CMD-check](https://github.com/edwbaker/SonicScrewdriveR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/edwbaker/SonicScrewdriveR/actions/workflows/R-CMD-check.yaml)

# Sonic ScrewdriveR

This package provides tools for manipulating sound files for analysis and visualisation.

The need for `SonicScrewdriveR` arose initially in the [Automated Acoustic Observatories](https://ebaker.me.uk/aoo) project at the [University of York](https://www.york.ac.uk), and later in the development of [audioBlast](https://audioblast.org). Both of these projects required the ability to read and manipulate audio files in a variety of formats and to visualise and analyse the data contained within them. The package is designed to be as flexible as possible and to be able to handle audio file formats with differing encoding, channel numbers, and sample rates. In one sense, it can be seen as a wrapper around existing packages to provide a unified interface, but it also offers many new functions and visualisations.

## Installation

NB: The current development version is well ahead of the version available on CRAN due to the pace of current research work as part of the Urban Nature Project at the Natural History Museum.

### CRAN

``` r
install.packages("sonicscrewdriver")
library(sonicscrewdriver)
```

### Latest master

``` r
install.packages("devtools")
devtools::install_github("edwbaker/SonicScrewdriveR")
library(sonicscrewdriver)
```

## Usage

Usage of the package is described in the following vignettes:

- [Managing audio in R with SonicScrewdriveR](https://sonicscrewdriver.ebaker.me.uk/articles/manage_audio.html) covers the basics of reading audio files with `readAudio()`, performing analyses over all audio channels using `allChannels()` and performing analysis on sections of audio with `windowing()`.

- [Using SonicScrewriveR with audioBlast](https://sonicscrewdriver.ebaker.me.uk/articles/audioblast.html) covers the use of `audioblast()` to find recordings, annotations, traits and analyses. `audioblastDownload()` can be used to download files you find using [audioBlast](https://audioblast.org).

The book [Visualisation for bioacoustics and ecoacoustics in R](https://av.ebaker.me.uk) provides details of the various visualisation tools available in this and other packages.

## Contributing

We welcome contributions from anyone who would like to help improve the package. If you want to talk to us before submitting a pull request feel free to email Ed at edward.baker@nhm.ac.uk.

Please note that the 'SonicScrewdriveR' project is released with a [Contributor Code of Conduct](https://github.com/edwbaker/SonicScrewdriveR/blob/master/CODE-OF-CONDUCT.md).

### Issue reporting

If you find a bug, or have a feature request, please report it on the [issue tracker](https://github.com/edwbaker/SonicScrewdriveR/issues) with as many details as possible.

If the package fails and does not give you a useful error message, this is a bug. If the package accepts invalid input without warning, this a bug. If the documentation is unclear or omits important information, this is a bug. If you are unsure if something is a bug, please report it.

## Credits

Initial development of [SonicScrewdriveR](https://sonicscrewdriver.ebaker.me.uk) was supported by the Leverhulme Trust funded [Automated Acoustic Observatories](https://ebaker.me.uk/aao) project at the [University of York](https://york.ac.uk).

The project is currently developed by the [Natural History Museum](https://www.nhm.ac.uk), London.
