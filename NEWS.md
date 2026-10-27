# sonicscrewdriver NEXT RELEASE

## New functionality
- audiomothExpand() expands an AudioMoth triggered (T.WAV) recording, in which
  the periods that did not trigger a recording are left out, restoring them as
  silence so that the timing of the triggered recordings is preserved. It gives
  either a Wave object or an ordinary wave file, and its output is identical to
  that of the reference implementation published by Open Acoustic Devices.
  readAudio() expands a file named following the T.WAV convention as it reads
  it, so existing code works on triggered recordings unchanged.
- audiomothCompress() writes a Wave as a T.WAV file, leaving out the segments
  whose peak amplitude falls below an amplitude threshold, as an AudioMoth
  device does.
- guano() reads GUANO metadata, the format used by many bioacoustic recorders
  and by current AudioMoth firmware, from a wave file or from a Wave object it
  is attached to. guanoCreate() and guanoParse() build it, guanoWrite() writes
  it to a wave file, and `guano<-` attaches it to a Wave. Fields the GUANO
  specification defines are converted to the types it gives them, and any other
  field is kept as written. Metadata is carried through expansion of a T.WAV
  file, and audiomothCompress() can write it.
- Wave methods for reticulate allow wave objects to be passed between R and Python
- Initial support for scikit-maad Python package
- dolbear() can now perform Dolbear's law calculations for more species
- audio_filesize() can output in human-friendly units using humanBytes()
- functions for sorting and merging Annotation objects
- fcis() calculates false-colour index spectrograms, in which three acoustic indices
  are calculated for each frequency bin of each time window and mapped to the red,
  green and blue channels of an image. Any three of the indices listed by
  fcisIndexNames() can be used, and there is a plot() method for the result.
- tagApply() applies a function to a TaggedWave and records the call in its
  processing slot: function, arguments, time, duration, and digests of the input
  and output. It also works on lists of waves, in parallel if given a cluster, and
  gives results that are not waves a processing attribute.  generateNoise(), concat() and filterWave() record their
  processes by calling it, so all processing records have the same form. WaveFilter objects have a func_name
  slot, so that the function a filter used is recorded by name.
- tagApply() gives functions from other packages, such as seewave, an
  untagged wave, as they do not accept TaggedWave and TaggedWaveMC objects, and
  puts the tags back on the result. filterWave() with bandpass() now works on
  tagged waves.
- normalise(), upsample(), cutws(), autoBandPass(), generateTimeMask() and
  generateTimeShift() record themselves on TaggedWave and TaggedWaveMC objects.
  Their results for Wave and WaveMC objects are unchanged, and are not tagged.
- generateTimeMask() given a list now passes dutyCycle and n.periods to each wave;
  previously they were dropped.
- addProcess() is no longer exported; use tagApply(). It now appends to the processing slot; previously each call nested
  the earlier history inside a new list. bandpass() filters now have a description.

## audioBlast
- audioblast() uses the current audioBlast API:
  - a filter can be given a vector of values, e.g. id = c(10754, 11096), and
    vectors of more than the 100 values audioBlast takes are sent in batches;
  - filters on a range, such as duration or lat, can be given a range made by the
    new ab_range();
  - page_size sets how many results come in each page, so that many results can
    be fetched in fewer requests;
  - columns that audioBlast describes as numeric are returned as numbers;
  - output = "JSON-LD" or "Turtle" returns results as linked data;
  - when audioBlast refuses a query, e.g. for a filter the module does not have,
    the error gives audioBlast's explanation;
  - check = TRUE works again for module endpoints, and module information is
    cached for the session.
- audioblastRecord() fetches a single record, as data or linked data.
- audioblastFilters() lists the filters an audioBlast module takes.
- audioblastDownload():
  - saves files in binary mode, which corrupted audio files on Windows;
  - skips files that already exist in dir, rather than in the working directory;
  - no longer fails when every file has already been downloaded;
  - names files by source and ID where their URL does not end in a file name,
    rather than saving every xeno-canto file as "download";
  - leaves no partial file behind when a download fails;
  - checks files against the SHA-256 hash audioBlast measured of them;
  - returns the paths of the files, invisibly.
- audioblast(on.issue = warning) gives a warning, as documented, rather than an
  error.

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

## Deprecations
- unit arguments are now named consistently across the package. A function takes
  input.unit for the unit a value is supplied in, output.unit for the unit a result
  is returned in, and <quantity>.unit (as in temp.unit, pressure.unit) where it
  takes several quantities that each need a unit. The former names still work and
  warn, and will be removed in a future release:
  - input became input.unit in convert2Pascals(), convert2dyne_cm2(),
    convert2Kelvin(), convert2Celsius(), convert2Fahrenheit(), convert2degrees(),
    convert2radians(), convert2bytes(), convert2seconds(), dielFraction() and
    yearlyFraction().
  - units became input.unit in readAudio() and readRespeaker6().
  - unit became input.unit in humanTime() and region().
  - unit became output.unit in referencePressure(), referenceIntensity(),
    wavelength(), dielFraction() and yearlyFraction().
  - the first argument of convert2seconds() was named T, which shadows the base R
    value TRUE. It is now named t.
  The unit argument of normalise() and data2Wave() is unchanged: it is passed to
  normalize() from tuneR, where it selects a bit depth rather than a unit of
  measurement.
- an unrecognised unit now raises an error listing the units that are recognised,
  so the accepted set does not have to be looked up in the documentation. The set
  is also checked before any conversion is attempted, which it previously was only
  in convert2seconds().

## Fixes
- readAudio() returned an empty wave for a file whose data chunk size was never
  written, which is what a recorder leaves behind when it loses power part-way
  through a file, discarding the whole recording the file still held. The audio
  of such a file is now read to the end of the file, with a warning.
  audiomothExpand() warns on a triggered recording left the same way. A file
  that really does hold no audio, such as one an AudioMoth writes when its
  battery fails as it starts to record, is still read as empty.
- readAudio() returned samples that were not finite numbers, which a
  floating-point file can hold, so that every measurement made from the wave
  came back as NaN. They are now read as zero, with a warning saying how many.
- soundSpeed() with method="cramer" used the value TRUE in place of the temperature
  in one term of Cramer's equation, because the term referred to an undefined
  variable named T. The speed of sound was therefore slightly wrong at every
  temperature other than 1 degree Celsius, by about 0.05 m/s at 14 degrees and
  standard pressure.
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
- every conversion function accepts its own output unit, so that a value already in
  the wanted unit passes through unchanged, but six of the seven did not say so:
  convert2Pascals() ("Pa"), convert2Kelvin() ("K"), convert2Celsius() ("C"),
  convert2degrees() ("degrees"), convert2radians() ("radians") and convert2seconds()
  ("seconds" and "POSIX"). convert2dyne_cm2() listed an input of "P", which it has
  never accepted, and did not list "dyne_cm2", which it does.
- dielFraction() gave its input as one of POSIX or HHMM, and yearlyFraction() gave
  its input as POSIXlt. dielFraction() accepts every unit convert2seconds() does,
  and yearlyFraction() accepts "POSIX" or "day".
- audio_filesize() gave its duration.unit as one of seconds, minutes, hours or days,
  although it accepts every unit convert2seconds() does, and one of its own examples
  uses "years". It also described humanBytes() output as the default when the
  default is bits.
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
