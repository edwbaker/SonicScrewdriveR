# sonicscrewdriver NEXT RELEASE

## New functionality
- Wave methods for reticulate allow wave objects to be passed between R and Python
- Initial support for scikit-maad Python package
- dolbear() can now perform Dolbear's law calculations for more species
- audio_filesize() can output in human-friendly units using humanBytes()
- functions for sorting and merging Annotation objects
- fcis() calculates false-colour index spectrograms, in which three acoustic indices
  are calculated for each frequency bin of each time window and mapped to the red,
  green and blue channels of an image. Any three of the indices listed by
  fcisIndexNames() can be used, and there is a plot() method for the result.
- humanBytes() and convert2bytes() understand binary file size units such as KiB and
  MiB, as well as the decimal kB and MB, selected with the units argument. The units
  available are listed by the new fileSizeUnits(). audio_filesize() passes the choice
  on when its output.unit is "human".

## Performance
- ste() with method="dietrich2004" now uses a cumulative sum rather than summing each
  window separately, making it around 80 times faster. This also speeds up
  pulseDetection() with method="dietrich2004" or method="simple", which both call it.
- pulseDetection() with method="dietrich2004" evaluates its detection state machine a
  run of samples at a time rather than sample by sample, making it around 7 times
  faster on top of the ste() improvement above.
- pulseDetection() with method="threshold" finds threshold crossings without a loop
  over every sample, making it 9 to 22 times faster. The larger the value of U, the
  larger the improvement.
- upsample() interpolates without a loop over every sample, making it 15 to 20 times
  faster.
- windowing() with bind.wave=TRUE joins the windows in a single call rather than one
  at a time. Joining them one at a time copied the whole of the output so far on
  every window, so the saving grows with the length of the recording: around 9 times
  faster for one minute of audio and 37 times for four minutes.

## Fixes
- audiomothWave() raised an error on recordings made with a band-pass filter. The
  two cut-off frequencies are now returned as filter.limit, as they already were
  for low-pass and high-pass recordings.
- ste() returned a vector of NA values when the wave was shorter than the analysis
  window, and the vector could be longer than the wave. It now returns zeroes.
- pulseDetection() with method="dietrich2004" recorded offsets as onsets, so the
  returned offsets were always empty and the returned onsets were a mixture of the
  two. Both are now correct, which also makes pulseStats() usable with this method.
- upsample() left NA values at the end of the upsampled wave, as the final sample has
  no following sample to interpolate towards. Its value is now held instead.
- windowing() with bind.wave=TRUE raised an error when only one window was analysed.
- windowing() with bind.wave=TRUE raised an error when passed a Wave object and a
  negative window.overlap, as the gap after the final window was requested as a
  section ending before it began. Passing a Wave object and passing a filename now
  give the same result.
- windowing() analysed a wave twice when its length was a single window, as the
  window start positions were counted down rather than up.
- channels_se() returned NULL for the ndsi() and acoustic_complexity() indices from
  the soundecology package, as it did not look for the names those functions give
  their results. It now supports every soundecology index, and raises an error rather
  than returning NULL when given something else.
- humanBytes() raised an error when given more than one size, as it tested them with
  a single if statement. It now returns one string per size, and gives NA for missing
  values. Sizes are rounded to three decimal places, which can be changed with the
  new digits argument.
- pulseDetection() with method="threshold" reported positions U samples later than
  they occurred, as the zero padding added to the start of the wave was not removed
  from the results. Positions now match those from the other methods.
- shimmer() measured the largest signed value of each half cycle rather than the peak
  amplitude of each period. A waveform of constant amplitude returned a large value
  that varied with its frequency, rather than zero, and the result was insensitive to
  the amplitude variation it was meant to measure. Its documentation described jitter
  and gave the wrong return value.
- jitter() measured the length of each half cycle rather than of each period. The two
  halves of an asymmetric waveform differ in length, so waveforms of perfectly
  constant period were reported as having a relative jitter of 40 per cent or more,
  and genuine jitter was reported at half its true value. Its documentation gave the
  wrong return value.

## Documentation
- convert2bytes() was described as converting time measurements into seconds, and its
  "bytes" input was undocumented.
- audiomothConfig() gave its return value as a data frame of matching annotations.
- yearlyPositions() was described as generating label positions for a dielPlot().
- corrected spellings of "processed", "metadata" and "rhythmicity" in the
  documentation of dielHistogram(), TaggedWaveMC and beatSpectrum().
- documented the return value of 46 functions that did not describe one, which R CMD
  check reports as a note.
- removed indexSpec(), which was never exported or documented, was not called
  anywhere, and defaulted to a noise file on the author's own machine.
- audioblastDownload() reports download problems with a warning, and gs_transcribe()
  reports downsampling with a message, rather than printing to the console. Both can
  now be suppressed in the usual way.

## Fixes
- readAudio() returns audio read through the av package (e.g. FLAC) at the bit
  depth of the source file, rather than at a bit depth inferred from the decoded
  samples. Audio read this way now compares equal to the same audio read from a
  WAVE file. Note that a 24bit file is returned as 32bit, as the av package does
  not report a bit depth of 24.
- readAudio() no longer leaves the sample_rate and channels attributes attached
  by av::read_audio_bin() on the samples of the Wave object it returns

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
