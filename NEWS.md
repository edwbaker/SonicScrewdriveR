# sonicscrewdriver NEXT RELEASE

## New functionality
- Wave methods for reticulate allow wave objects to be passed between R and Python
- Initial support for scikit-maad Python package
- dolbear() can now perform Dolbear's law calculations for more species
- audio_filesize() can output in human-friendly units using humanBytes()
- functions for sorting and merging Annotation objects

# sonicscrewdriver 0.0.7

## New functionality
- sweptsine() can generate logarithmic sweeps with mode="log"
- normalise() wrapper to tuneR::normalize() to detect bit depth
- data2Wave() uses new normalise() wrapper

## Documentation
- additional examples for sweptsine()

## Fixes
- internal function to normalise a spectrum
- better testing of bind.wave parameter to windowing().


# sonicscrewdriver 0.0.6

## Fixes
- issued identified on Win build
- convert2seconds() where numeric value < 1000 passed as HHMM.


# sonicscrewdriver 0.0.5

## New functionality
- use BirdNET-Analyzer within R using reticulate
- readAudio() generic function to read audio files
- readAudacityLabels() to read Audacity label files
- citation() information
- angle conversion
- Dolbear's law
- audioblast() - search for recordings and analyses on audioblast.org
- audioblastDownload() - Download files from audioBlast
- allChannels for analysing multi-channel audio
- read Respeaker6 formatted files

## New classes
- Annotation class (recording and manipulating annotations)
- PseudoWave class (e.g. apply noise to a file of any length)
- TaggedWaves class (document metadata and processing)
- WaveFilter class (supports applying filters using pipes)

## Fixes
- audiomothWave() fix (comments not consistently placed in header)


# sonicscrewdriver 0.0.4

- support for reading AudioMoth configuration files
- support for reading AudioMoth metadata in wave files
- audioRead function to convert more audio file types to Wave object
- parseFilename now accepts POSIX timestamp as a format
- provides some typical volumes (in decibels)

# sonicscrewdriver 0.0.3

- Jitter and shimmer functions added.
