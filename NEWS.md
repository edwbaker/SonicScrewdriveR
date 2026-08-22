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
- writeAudio() writes a Wave or WaveMC object to a file, and is the counterpart of
  readAudio(). WAVE files are written directly, and any other format the encoders
  available to the av package can produce (e.g. FLAC, MP3) is written by converting
  a temporary WAVE file. FLAC can also be written without the av package.
- tdoa() measures the time differences of arrival of a sound between the channels of
  a multichannel recording, by generalised cross-correlation against a reference
  channel. The phase transform (the default), unweighted cross-correlation, or
  correlation of the Hilbert amplitude envelopes can be used, and the delay found can
  be interpolated to a fraction of a sample. The envelope method is the one to use
  where the waveform does not stay coherent between microphones.
- bearing() estimates the direction a sound arrived from, given the time differences
  of arrival tdoa() measures and the positions of the microphones. Arrays that
  cannot give a single answer, such as a pair of microphones, report every direction
  they allow, and delays that no sound travelling at the speed of sound could have
  produced are reported as such.
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
- readAudio() returns audio read through the av package (e.g. FLAC) at the bit
  depth of the source file, rather than at a bit depth inferred from the decoded
  samples. Audio read this way now compares equal to the same audio read from a
  WAVE file. Note that a 24bit file is returned as 32bit, as the av package does
  not report a bit depth of 24.
- readAudio() no longer leaves the sample_rate and channels attributes attached
  by av::read_audio_bin() on the samples of the Wave object it returns.
- corWaveMC() now returns the time differences between channels that its
  documentation has always promised. Each event is now a list holding the corenv
  results for every channel in `correlations` and the delays in `delays`, where
  before it was the corenv results alone, so code reading the result needs updating.
  Arguments are also now passed on to corenv(), which matters most for its `method`:
  the rank correlation it uses by default can report no offset at all for a recording
  that is quiet between its events. Its `temp` argument, which it has never used for
  anything, has been removed.
- jitter() returned NULL invisibly when given a method it did not recognise, rather
  than reporting the mistake. It now raises an error naming the method.
- dielPlot() drew the nadir marker without applying the plot's rotation, so it was
  placed correctly only for the default rotation, where a half turn either way comes
  to the same thing. It is now rotated like every other band on the plot.
- convert2Kelvin(), convert2Celsius() and the validators behind them raised an error
  when given more than one value, as they tested a single if statement. They now
  return one result per value.
- humanTime() raised an error when any of the times given was NA, rather than
  returning NA for it as humanBytes() already did.
- validateRH() misspelled "Relative" in the error it raised for a humidity outside
  0 to 100.
- convert2Pascals(), convert2dyne_cm2() and convert2degrees() and their counterparts
  now share one table of conversion factors for each family of units, so a factor
  cannot be right in one direction and wrong in its inverse.
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
- humanTime() raised an error when given more than one time, for the same reason as
  humanBytes(). It now returns one string per time, and rounds to three decimal
  places, which can be changed with the new digits argument.
- soundSpeed() with method="cramer" used the constant T, which R resolves to TRUE, in
  place of the temperature in the pressure term of Cramer's expression. Speeds were
  biased by 3.73e-8 * (1 - t) * P, which is -0.11 m/s at 30 C and standard pressure,
  and the sign of the error reversed below 1 C. The calculation now reproduces the
  published coefficients exactly, and is tested both against them and against the
  quoted speed of sound in dry air at 0 and 20 C.
- soundSpeed() with method="cramer" took its default mole fraction of CO2 as 400^-6,
  which is 2.4e-16, rather than 400e-6, the 400 ppm nominal composition used by
  Cramer. The default is now 400 ppm, which changes results by around 0.04 m/s. The
  validity ranges given in the paper are now documented.
- dayPhase() and dayPhases() raised an error on every call, as the moon's state was
  built into the same vector as the phase boundaries and a time cannot be combined
  with a logical. The state of the moon is now returned as an attribute, and the
  phases as a data frame of start and end times.
- dayPhase() looped forever whenever a recording ran past the end of the phases of
  its first day, as each pass through the loop fetched the same day again. The days
  a recording covers are now calculated directly.
- dayPhase() dropped any phase that the recording ended part way through, as the
  three cases it tested did not include that one. It now keeps every overlapping
  phase.
- dayPhase() did not limit relative end times to the length of the recording, as it
  compared a time relative to the start of the recording against an absolute time.
- dayPhases() reported the moon setting before it rose, as suncalc gives the rise and
  set falling within one calendar day. The moon now ends at the following day's set
  where that is the one which ends its time in the sky.
- dayPhases() raised an error on around one day in fifteen, being those where the
  moon does not both rise and set on the same day, which suncalc::getMoonTimes()
  cannot calculate for a single date.
- daysPhases() ignored its date argument and always used the current date.
- daysPhases() raised an error for period="month", as it asked for a year of moon
  times to go alongside a month of sun times.
- daysPhases() labelled the columns of its result by position, which named civil dusk
  as nautical, nautical dusk as astronomical, and returned no astronomical dusk at
  all. Columns are now named from the values they hold.
- daysPhases() gave an error naming an internal variable when passed a period other
  than "month" or "year".
- autoBandPass() rejected its own default value of n.bw, as a whole number written
  without an L is a double and the check was for storage type rather than value. An
  unrecognised bw now gives an error naming it, rather than one about a missing
  object.
- dielHistogram() drew bins of 6 and 12 minutes when asked for 15 and 30, and gave a
  cryptic error for any other value of by.
- windowing() raised a subscript error when no complete window fitted in the wave,
  as the test for whether to bind the results looked at the first of them before
  checking there were any.
- pseudoWave()'s example for type="file" named a file that cannot exist, so R CMD
  check failed on it. It is now marked as not run.
- TimeRegion regions given in minutes or hours started at the sample for that many
  seconds, as the multiplier was applied only to the end of the region. Regions
  given in samples now start at sample one, as they do for every other unit, and an
  unknown unit gives an error.
- merge_annotations() kept only the start of the earlier annotation, so merging one
  annotation into another that contained it discarded the longer one's end. The
  merged annotation now spans both. The same applied to frequency limits. An empty
  list no longer raises an error, and an unknown domain now gives one.
- AnnotationList2DataFrame() raised an error when no annotation had any metadata.
- labelReduction() raised a subscript error, and duplicated any label wholly inside
  another, as two of its three merge cases read the regions accumulated so far using
  the index of the input. It also failed for a transcript holding a single timing.
- writeAudacityLabels() wrote frequency limits only when they held no information,
  and named the columns of the five column form so that seewave could not find them,
  giving a file with neither times nor frequencies. readAudacityLabels() read the
  frequency rows of such a file as labels in their own right, and now returns the
  frequency limits it finds.
- pulseIntervals() ignored its nsd argument, wrote its results at the index of the
  interval being examined so that the output was padded with zeroes, and paired each
  gap with the onset before the one it started at. It also failed for fewer than
  three onsets.
- frequencyStats() read the lower edge of the threshold region one bin below the
  region itself, making every bandwidth one bin too wide and every centre half a bin
  too low. Tied maxima in the spectrum returned every tied bin where one frequency
  was meant, giving vectors for peak and Q. A spectrum with no signal above lowcut
  now says so rather than giving an error about the sample rate.
- pulse() took its amplitude from the bit slot of the wave it had created rather
  than from the unit asked for. As tuneR resolves bit=1 to a wave whose slot reads
  32, every pulse that was not 8-bit was far outside the range its format allows and
  writeWave() refused to write it. A square pulse of no length wrote two samples.
- filterWave() returned only the first channel of a stereo Wave or a WaveMC, as the
  filters read the left slot. Each channel is now filtered and the result put back
  together with the same class and channel count as the input.
- filterWave() raised an error for TaggedWave and TaggedWaveMC objects, as the
  filters do not know that class and addProcess() has no method for the plain wave
  they return. The tags are now carried across the filter.
- addProcess() nested the processing history rather than appending to it, so the
  shape of the slot changed with every process added.
- gs_transcribe() rewrote the sample rate in the header of a file it described as
  downsampled, without resampling it, which slowed the audio down, dropped its pitch
  and changed its duration. It also wrote that file into the working directory.
- gs_transcribe() polled for its results in a loop with no test for completion, so
  it never returned. It now stops when the transcription has finished, and gives up
  after max.tries attempts.
- parseFilename() read a timestamp against an epoch taken in whatever timezone the
  session was in, and ignored its timezone argument, so the time was out by the
  local offset of the machine reading it.
- generateNoise() with noise.ref="max" took its reference level from the bit slot of
  the wave, which reads 32 for a floating point wave, so the noise was referenced to
  two thousand million rather than to one. The two settings of noise.ref now agree
  for a wave that is already at full scale.
- yearlyPlot() and dielPlot() built their days as times in whatever timezone the
  session was in and then read them back as UTC, so a session east of UTC plotted
  the sun times of the day before.
- yearlyPlot() drew the wrong shape at latitudes where the sun does not rise or set,
  as the missing sunrise and sunset reached polygon(), which reads a missing
  coordinate as a break between sub-polygons. Such a day is now taken as a whole day
  of light or of darkness according to where the sun is at noon.
- yearlyPlot() placed the first day of the year at the same angle as the origin and
  then the second a whole extra day around the circle.
- yearlyPlot() labelled its months with the positions of the year 2022 whatever year
  it was asked for, so the labels of a leap year were a day out.
- yearlyPlot() now draws the legend its legend argument asks for, as dielPlot() does.
  Its plot argument is still not implemented, and now says so.
- yearlyPositions() with format="mid-months" measured the last month against a 360
  day year, so the December label sat outside December.
- dielPositions() with format="hours" or "minutes" had no position for midnight and
  repeated it at the end as 2*pi.
- dielRings() drew every ring 0.1 thick however many were asked for, so more than ten
  rings overlapped one another and ran past the outer limit. Rings now fill the
  region between the limits. An empty set of rings now draws nothing rather than
  raising an error.
- dielRings() and dielHistogram() now take the rot argument of the dielPlot() they
  are drawn on, without which they were drawn at the default rotation and did not
  line up.
- radialPolygon() drew nothing when given the same angle to start and end at, which
  is how a ring covering a whole day is described. It now draws the full circle.
- frequencyStats() described its warning about a degenerate threshold region the
  wrong way round, saying the maximum was greater than or equal to the minimum when
  that is the ordinary case. The warning now also fires where it should, as reading
  the lower edge a bin early had made every region at least one bin wide.
- soundSpeed() returned the speed of sound in air, as though nothing were wrong,
  when given a method it did not recognise, and returned NULL for a set of arguments
  that named no calculation at all. Both now give an error.
- soundSpeed(medium="all") returned the speeds as strings, as they were combined
  with the names of the media using cbind(). The same applied to typicalVolume(),
  whose dBA column was character.
- typicalVolume() raised an error about the length of a condition when given more
  than one thing to look up, and now accepts a vector.
- tSamples() divided by the duration of a single sample rather than multiplying by
  the sample rate, and the extra rounding left it a sample short of an exact
  duration.
- zerocross() reported every sample equal to zero as a crossing, so a run of zeros
  gave one crossing per sample and a zero between two samples of the same sign gave
  a crossing that never happened. A run is now one crossing, and only where the
  signal changes sign across it.
- upsample() never called a function given as its method, which is the documented
  way of interpolating, and returned a wave of alternating NA values for any method
  it did not recognise rather than saying so.
- ntd() counted backwards, indexing its events with NA, when given fewer than two.
- concat() returned the first of its arguments unchanged when given a mixture of
  Wave and WaveMC objects, as neither branch matched.
- Subsetting a Wave with a TimeRegion padded the result with NA when the region
  reached past the end of the wave. Only an infinite end was clamped.
- dutyCycle() returned NULL for an output format it did not recognise.
- allChannels() returned NULL rather than an error for input that is not a wave.
- cleanTZ() returned NULL for any timezone not written as a UTC offset.
- convert2seconds() accepted partly numeric HHMM input, which then failed with an
  unrelated message, and gave an error naming an internal variable when given an
  origin other than "day" or "unix".
- validateFreq() raised a subscript error for a frequency vector of length zero.
- The warnings from setting pcm named an argument that does not exist.
- The PseudoWave class prototype and the pseudoWave() constructor disagreed about
  the default seed, the one giving no seed and the other a seed of 1. Both now give
  a seed of 1, so that a PseudoWave repeats its samples unless asked otherwise, and
  a seed of NA asks for a fresh sequence each time. NA is now accepted whether it is
  written as a bare NA or as NA_integer_.
- A PseudoWave given a seed set the random stream of the whole session, changing
  every random number drawn afterwards. The stream is now put back.
- A PseudoWave built from a file was read whole and then recycled against whatever
  wave it was used with, however badly the two lengths matched. It now gives exactly
  as many samples as the wave it meets, repeating or trimming the file as needed.
- pseudoWave() accepted a type it cannot generate, which then failed with "object
  'w' not found" when the PseudoWave was used. Both the constructor and the
  conversion now name the unsupported type.
- The PseudoWave class documented a slot it does not have.
- readRespeaker6() with header=TRUE always raised an error, as readWave() then
  returns a description of the file rather than a wave with channels to select.
- rainfall_bedoya2017() calculated its statistics before checking there were enough
  data for them, and only guarded against a band of exactly one row, so a band
  holding nothing at all compared NaN against the thresholds.
- generateTimeMask() gave every member of a list the default duty cycle and period
  count, whatever was asked for, and returned a list that was not of waves unmasked.
- generateTimeShift() with type="rotate" asked for a section running backwards when
  the shift was longer than the recording, so the default failed on anything shorter
  than two seconds. A rotation now wraps, as a rotation should.
- pulseDetection() analysed the left channel alone, without saying so, and could not
  take a WaveMC at all. Multi-channel input now goes through allChannels(), as it
  does for dutyCycle() and rainfallDetection().
- ste() had no default window length, so its own documented example failed with a
  message naming an argument documented nowhere, and returned NULL for a method it
  did not recognise.
- dolbear() paired a vector of values off against the populations in its table, so
  each value was tried against a different regression line. Every value is now
  calculated for every population.
- birdNetAnalyse() read NA for the year, month and day when given a list of dates,
  and grew its results one detection at a time.
- maad_frequency_entropy() accepted a maad object and then replaced it, unlike every
  other index in the file.
- maad_spectral_entropy() had documentation and a help page but was not exported, so
  it could not be called, and raised an error on every call once it could. The
  frequencies of the spectrogram reached Python as a list rather than an array, so
  scikit-maad's own default band, which is taken from their smallest and largest
  values, had no minimum to ask for. A band given to flim was passed on as a list
  as well, which scikit-maad accepts neither as a pair nor as an array, and left it
  without the band it was about to use. Its results now match those of scikit-maad
  called directly, with or without a band, and it is tested.
- scaleRGB() turned a whole colour channel black when a single value was missing.
- naturalFrequency() and resonantFrequency() raised an error about the length of a
  condition when given more than one value, as the default capacitance was signalled
  by a string.
- directionPlot() raised an error for every input, as the row it appends to close
  the polygon was built with names that matched nothing.
- readBirdNet() returned NULL for a path that is neither a file nor a directory.
- audiomothConfig() left a space at the front of every value, so comparing one
  against a string never matched.
- corWaveMC() cut its reference channel again for every channel of every event, and
  described a return value it never calculated.
- The documentation of convert2dyne_cm2(), data2Wave(), audio_filesize() and
  addSpectra() described units, ranges, defaults and an example that did not match
  what the functions do.
- pd_simple() returned a threshold other than the one it found its pulses with, so
  the threshold given back did not reproduce the pulses given back. It now returns
  both, named as pulseDetection() with method="dietrich2004" names them.
- pulseStats() counted backwards when given fewer than two pulses, and assumed a
  recording never ends part way through one.
- sDuration() and tSamples() silently ignored samp.rate when a wave was given as
  well. Giving both is now an error, as it is for validateFreqIsPossible().
- emptyYearly() took a method argument spelled "plotix" that it never used, and now
  gives an error for a method it does not know.
- validateTimeInSeconds() compared only the last of several times against max_t, as
  the comparison was made outside the loop over the times but still used the loop
  variable. An over-long time anywhere else in the vector passed silently, and
  coerceMaximum=TRUE left it unchanged. This affected labelPadding(), which could pad
  a label beyond the end of the recording. Every time is now checked.
- specStats() raised an error when given a single spectrum, as it validated the
  spectra against each other starting from the second one. It also built the plot
  inside that validation loop, recalculating the statistics for every spectrum and
  discarding all but the last plot, and both printed and returned the plot so that
  it was drawn twice. It now validates, calculates and plots once, and raises a clear
  error for an unknown value of stats or for input that is not a list of spectra.
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

- validateSpectrum() aborted with R's own "missing value where TRUE/FALSE needed"
  when a spectrum held an NA in its frequency column and NAs were being coerced. It
  now says which column the NA was in. NA amplitudes are still coerced to zero.

## Internal
- validateFreqIsPossible() and validateSpectrum() now test their input in one
  vectorised pass rather than looping over each frequency, or each cell of the
  spectrum, in turn.
- The option arguments the package validates, the physical-quantity validators, the
  unit conversions, the human-readable size and time formatting, the -3dB and -10dB
  halves of frequencyStats(), the twilight bands of dielPlot(), the four TimeRegion
  constructors, the optional maad argument of the scikit-maad wrappers and the polar
  plot legends are each now written once rather than repeated at every call site.
  Behaviour is unchanged except where noted under Fixes.
- generateTimeShift() no longer takes an output argument. It accepted only the value
  "list" and never used it.
- The tests that query the audioBlast API now skip when it cannot be reached, rather
  than failing. The API answers R's default user agent with a 403, so R CMD check
  failed on any machine behind that.

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
