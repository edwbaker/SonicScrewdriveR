# sonicscrewdriver NEXT RELEASE

## New functionality
- readMetadata() reads the embedded metadata of an audio file, whatever format
  that metadata is in and whatever container the file is, and gives back both
  what each format said and one normalised record built from all of them. A
  recording commonly carries several of these formats, written by the device
  that made it and by whatever software has handled it since, and the same fact
  under a different name in each; the record holds the time, make, model,
  serial, firmware, title, species, recordist, comment, licence, position and
  elevation the file gave, taken from the most specific format that gave each.
  Chunks carrying metadata this package cannot read are named rather than
  passed over in silence, so that a file is not taken to carry nothing just
  because nothing here can read what it carries.
- `audioMetadata<-` attaches a reading to a wave, which readMetadata() reads
  back. A wave keeps the metadata of its file in one place: a TaggedWave has a
  metadata slot made for this, so the reading goes in there under the name
  "audioMetadata", beside anything else the caller has recorded in that slot,
  and a plain Wave, which has nowhere of its own, carries an attribute of the
  same name. `guano<-` now writes into that same place rather than an attribute
  of its own, so a wave cannot end up carrying one record of its file in one
  place and another somewhere else, and reading a section of a recording or
  expanding a triggered one carries everything its file said over rather than
  only its GUANO metadata.
- The formats readMetadata() reads can each be read on their own: id3() reads
  the ID3 tag of an MP3 file, or the one a wave file holds in a chunk, in
  versions 1 and 2.2 to 2.4; wamd() reads the Wildlife Acoustics chunk that
  Song Meter recorders write; bext() reads Broadcast Wave metadata, whose
  description is where a sound library commonly puts the whole of its record
  for a recording; wavInfo() reads the INFO list, which is where AudioMoth
  firmware records the settings a recording was made with, and the labels an
  editor has given the cue points of a recording; ixml() reads the
  iXML chunk field recorders write; and vorbisComment() reads the comments of
  a FLAC or Ogg file. Each takes a path, a Wave object or the bytes of a file,
  reads only the metadata rather than the whole recording, and gives an empty
  result rather than an error for a file that carries none.
- xmp() reads the XMP packet a wave file carries in its `_PMX` chunk, the
  metadata format of Adobe's audio software, and disp() reads the DISP chunk,
  the clipboard item holding the name a file is to be shown under. Both were
  among the chunks readMetadata() could only name, and both carry real records
  in a cached copy of bio.acousti.ca: 113 of those files hold a packet
  describing the recording as an editor read it, and nineteen hold a display
  name giving the place a recording was made. XMP allows a property to be
  written as an attribute, as an element or as a list of alternatives, and all
  three are read; a property this package has never heard of is read as readily
  as one it knows. A DISP chunk holding nothing but the number of its clipboard
  format, which is how most writers that mention the chunk leave it, gives
  nothing rather than an empty title, and one holding an image names the format
  so that a caller can tell the file carries something other than text.
- A file is identified by its bytes rather than by its name, so an MP3 or an
  Ogg file under a .wav extension is read as what it is.
- parseFilename() reads the filename convention of the TSA corpus and the
  collections distributed with it, as
  `TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav`, giving the collection,
  the taxon, the accession number the collection gave the recording and the
  hash of the file. readMetadata() reports the taxon as the species and the
  collection beside it, which for much of that corpus is the only record of
  what was recorded: those files commonly carry no embedded metadata at all.
  A taxon is read only from a scientific name, so the part of the corpus named
  by a German common name and a German word for the kind of call gives none
  rather than a taxon invented from its first two words.
- parseFilename() says when a reading was a guess. `ambiguous` is TRUE and
  `alternatives` names every format the filename could have been in, where the
  name itself could not settle it. A name of nothing but digits is the case
  that matters: eight digits are read as a date and ten as a count of seconds,
  and a collection that numbers its records gives names of exactly that shape.
  readMetadata() therefore takes no date from such a name, since nothing
  outranks a filename and a confident wrong date is worse than none; a format
  asked for by name is not a guess and carries no alternatives.
- readMetadata()'s `filename` argument takes the name of a filename format as
  well as TRUE or FALSE, as in `filename = "YYYYMMDD"`. Where you know what
  your own files are named by, naming the format settles what the digits mean
  and the date is read and used rather than passed over as a guess. A format
  the package does not know is an error at once rather than a file quietly
  reading as though nothing had been asked for.
- Text declared as ISO-8859-1 that is in fact valid UTF-8 is read as UTF-8,
  which is what xeno-canto writes into the comment frame of its recordings.
- audiomothExpand() expands an AudioMoth triggered (T.WAV) recording, in which
  the periods that did not trigger a recording are left out, restoring them as
  silence so that the timing of the triggered recordings is preserved. It gives
  either a Wave object or an ordinary wave file, and its output is identical to
  that of the reference implementation published by Open Acoustic Devices.
  readAudio() and windowing() expand a triggered recording as they read it, so
  existing code works on them unchanged. One is recognised by the trigger
  AudioMoth records in the comment it writes into the header, rather than by
  the T before its extension, as a name is only a convention: an ordinary
  recording can meet it and a triggered one can be renamed out of it.
- audiomothCompress() writes a Wave as a T.WAV file, leaving out the segments
  whose peak amplitude falls below an amplitude threshold, as an AudioMoth
  device does, and records that threshold in the file's comment as a device
  records it.
- guano() reads GUANO metadata, the format used by many bioacoustic recorders
  and by current AudioMoth firmware, from a wave file or from a Wave object it
  is attached to. guanoCreate() and guanoParse() build it, guanoWrite() writes
  it to a wave file, and `guano<-` attaches it to a Wave. Fields the GUANO
  specification defines are converted to the types it gives them, and any other
  field is kept as written. Metadata is carried through expansion of a T.WAV
  file, and audiomothCompress() can write it. Given a path, it walks the chunks
  of the file over a connection and reads only the metadata chunk, so reading a
  field from a recording of any length needs no more memory than the metadata
  itself.
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
- readAudio() reads files with more than two channels, returning a tuneR::WaveMC
  object holding one column per channel. Reading such a file through the av package
  (e.g. a multichannel FLAC) used to stop with "channel count greater than 2 is not
  supported". Cutting the result with from and to keeps every channel, whether the
  section is given in samples or in time units. Mono and stereo files are returned
  as a Wave object as before.
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
- The warnings readAudio() raises about the file it read each carry a class of
  their own, so that one of them can be handled without silencing the rest:
  `sampleRateMismatch` for a header that disagrees with the recorder's own
  metadata, `unwrittenAudioSize` for a recording whose size was never written,
  `nonFiniteSamples` for samples that are not finite numbers, `badDs64` and
  `frameCountMismatch` for an RF64 or BW64 file whose ds64 chunk cannot be
  believed or miscounts its sample frames, and `truncatedAudio` for a recording
  that was cut short, which audiomothExpand() raises as well. A corpus known to
  hold truncated recordings can now pass over that one warning and still hear
  about the others, which matching the wording of a message could not do. All
  of them are also classed `audioFileWarning`, for handling every warning about
  the file at once, and `warning`, so `suppressWarnings()` and an ordinary
  warning handler treat them as the warnings they are.

## Deprecations
- unit arguments are now named consistently across the package. A function takes
  input.unit for the unit a value is supplied in, output.unit for the unit a
  result is returned in, and <quantity>.unit (as in temp.unit, pressure.unit)
  where it takes several quantities that each need a unit. The former names still
  work and warn, and will be removed in a future release:
  - input became input.unit in convert2Pascals(), convert2dyne_cm2(),
    convert2Kelvin(), convert2Celsius(), convert2Fahrenheit(), convert2degrees(),
    convert2radians(), convert2bytes(), convert2seconds(), dielFraction() and
    yearlyFraction().
  - units became input.unit in readAudio(), readRespeaker6() and tdoa().
  - unit became input.unit in humanTime() and region().
  - unit became output.unit in referencePressure(), referenceIntensity(),
    wavelength(), bearing(), dielFraction() and yearlyFraction().
  - the first argument of convert2seconds() was named T, which shadows the base R
    value TRUE. It is now named t, as it already was in dielFraction() and
    yearlyFraction().
  The unit arguments that do not name a unit of measurement are unchanged: the one
  normalise() and data2Wave() pass to normalize() from tuneR selects a bit depth,
  and the units argument of humanBytes(), convert2bytes() and audio_filesize()
  chooses between decimal and binary prefixes. The unit element of the list
  bearing() returns is part of its return value rather than an argument, and keeps
  its name.
- the documented set of accepted units was incomplete in several places, and is now
  the set each function actually takes: every convert2*() function accepts its own
  output unit, so that a value already in the wanted unit passes through unchanged;
  convert2dyne_cm2() listed an input of "Pa" and "kPa" but not "dyne_cm2";
  convert2seconds() did not list "seconds" or "POSIX"; dielFraction() gave its input
  as POSIX or HHMM, where it accepts every unit convert2seconds() does; and
  yearlyFraction() gave its input as POSIXlt, where it accepts "POSIX" or "day".
- while a former name and the argument that replaced it are both arguments, an
  abbreviation of the former name that also abbreviates the new one matches both
  and raises an error rather than being guessed. This affects the former input
  only, which is a prefix of input.unit, so `convert2Pascals(1, inp="kPa")` is now
  an error where it used to be accepted; the name in full, and any abbreviation
  of the new name, both work. The former unit and units are unaffected, as
  neither is a prefix of the argument that replaced it.

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
- guanoWrite() copies the recording across a buffer at a time rather than reading
  it into memory to rewrite it. Replacing a few hundred bytes of metadata needed
  an allocation several times the length of the recording, which fails on a
  machine that cannot find it: changing one field of a 300MB recording peaked at
  1.9GB, and now takes no more memory than changing it in a short one, whatever
  the recording is.

## Fixes
- guano() and guanoParse() kept as text, with a warning, the timestamp a
  Wildlife Acoustics Song Meter Mini Bat writes, 2025-09-15 21:31:57+2:00, so
  it never became a time a caller could compute with. It departs from the
  specification twice, with a space between the date and the time where a T
  belongs and an offset hour of a single digit. Both are now read, as is an
  offset with no colon, with a message naming what departs from the
  specification, and the value is written back in the form the specification
  gives. readMetadata() fared worse with it, as the text was read as a date and
  time in the zone of the machine, with the offset after it dropped, and now
  gives the moment the recording was made. A timestamp with an offset from UTC
  is written back with that offset, as the specification recommends: one of
  +01:00 was written back moved to UTC, as Z, so changing any other field of the
  metadata lost the offset the recorder was set to.
- readAudio() given a start point at or beyond the end of the recording failed
  in words that said nothing of what had been asked for, and in different words
  for different units and formats: "'from' cannot be superior to 'to'" in
  seconds, "In cutws to must be greater than from" in samples, and "subscript
  out of bounds" for a start at the very end of a FLAC file, where a WAVE file
  gave an empty wave instead. Such a start is now an error of class
  `startPastEnd` whatever the format and the unit, saying how long the
  recording is and what start was asked for: "The start point asked for, 1
  second, is at or beyond the end of 'f.wav', which is 0.5 seconds long." A
  start of zero is still the start of any recording, so a file holding no audio
  is read from there as empty.
- readAudio() given an end point that left no audio after the start point gave
  a different answer for each format and unit. An end before the start failed
  with "'from' cannot be superior to 'to'" in seconds and "In cutws to must be
  greater than from" in samples. An end at the start, `from = 0.3, to = 0.3`,
  gave an empty wave from a WAVE file but two samples, in reverse order, from
  FLAC, MP3 or a triggered AudioMoth recording; in samples an end just before
  the start, such as `from = 0, to = 0`, gave an empty wave from WAVE and an
  error from the rest. Each of these is now an error of class `endBeforeStart`
  whatever the format and the unit, saying what was asked for: "The end point
  asked for, 0.2 seconds, is before the start point, 0.3 seconds, so no audio
  of 'f.wav' was asked for." In samples the end point is the last sample
  returned, so an end at the start still gives that one sample.
- readAudio() cut a WAVE file a sample away from every other format where a
  start time fell half way between two samples, as `tuneR::readWave()` rounds
  such a time differently from `seewave::cutw()`: two parts asked for either
  side of such a time lost a sample between them or both held it. Every format
  is now cut at the same samples, and two parts meeting at a time meet without
  a sample lost or repeated. A wave file whose format is unknown (0), which
  `tuneR::readWave()` reads as PCM, is now held against the audio it holds in
  the same way, so a start past its end and an end before its start are
  refused as they are for any other file.
- guano(), wavInfo(), bext(), ixml(), wamd(), id3() and readMetadata() gave
  nothing for a wave file in the RF64 or BW64 form, which is how a recording is
  written once it passes the 4 GiB a RIFF header can describe, so the metadata
  of a long recording was reported absent. guanoWrite() refused such a file, and
  readAudio() read one whose sizes were never written as empty, as the av
  package, which reads these forms, takes those sizes at their word. The forms
  keep their sizes in a ds64 chunk, which is now read, and the sizes a write
  changes are written there, so a file stays in the form it was in. A ds64 chunk
  that is missing, comes later than first, or disagrees with the file, as the E2
  file of the malformed wave files reference set does, is refused rather than
  guessed at. readAudio() reads the audio of such a file as the av package does,
  by the sizes in its ds64 chunk where it has one, and says what is wrong with
  it. A count of sample frames that disagrees with the audio is read past with a
  warning, and put right in a file written.
- readAudio() chose its reader by the extension of the file, so a file under
  the wrong one was read as the wrong format. A wave file called .mp3 was
  given to `tuneR::readMP3()`, which does not refuse it: it finds a frame
  header somewhere in the PCM and decodes from there, and a 44.1 kHz mono
  recording of 43.6 seconds came back as 9.8 seconds of 22.05 kHz stereo with
  nothing raised. An MP3 called .wav was read by the av package instead of by
  `tuneR::readMP3()`, returning the same audio at a different bit depth and
  length than the same file under its own name. The reader is now chosen from
  the first bytes of the file, which say which container it is, and the name
  used only where those bytes identify nothing. Passing `mime` is still taken
  as given.
- defaultCluster() asked for a cluster of NA workers on a machine where
  parallel::detectCores() cannot count the cores, which it is documented to
  report as NA. One was subtracted from the count and the result held at a
  minimum of one with max(), but NA - 1L is NA and max() keeps it, so the NA
  reached makeCluster() and failed there with "numeric 'names' must be >= 1",
  naming neither the function nor the cores. Such a machine now gets a cluster
  of one.
- pythonInstall() stopped with "the condition has length > 1", which names
  neither Python nor pip, where the pip of an existing `sonicscrewdriver`
  environment could not be imported. reticulate asks pip for its version before
  installing into an environment, and compares what comes back against a
  minimum; where the import fails, that comparison is handed the three lines of
  a Python traceback in place of a version. pip being installed but not
  importable is what an interrupted install leaves behind, and what a
  file-syncing service does to an environment built inside a synced folder,
  carrying off the loose files of the package and leaving its directories. The
  import is now attempted first, and such an environment reported as needing a
  rebuild.
- bext() gave midnight for a recording whose origination time was written as
  hours and minutes, rather than the hour and minute the file gave. The field
  has eight characters for a time and the specification asks for seconds in
  them, but writers leave the seconds off and pad the rest, and the time is
  there to be read. 24 of the 241 Broadcast Wave files of the BioAcoustica
  corpus are written this way. A time that is unreadable for any other reason
  still gives the date at midnight rather than nothing, as before.
- readBirdNet() took the name of a file by splitting its path on a forward
  slash, so on Windows, where a path is separated with backslashes, the whole
  path was read as the name. filename_parsing="audiomoth" and "timestamp" then
  parsed the directories rather than the name and gave NA start and end times
  for every detection. The name is taken with basename(), which reads either
  separator.
- readAudio() failed on an AudioMoth triggered (T.WAV) recording with "argument
  "units" is missing, with no default". The renaming of the unit arguments left
  the call that cuts an expanded recording passing the deprecated argument,
  which has no default, rather than the one that replaced it.
- naturalFrequency() and resonantFrequency() documented their default capacitance
  as the IUPAC standard pressure, in SI units, but used 100 rather than 1e5
  Pascals, so supplying the documented value explicitly gave a different answer
  from leaving it out, and in SI units gave NaN. The default is now 1e5 Pascals,
  and is shown in the argument list rather than standing behind a sentinel value.
  Results that relied on the old default change: pass C=100 to keep them.
- naturalFrequency() returned NaN for an overdamped system, in which the damping
  term exceeds the stiffness term and there is no real natural frequency. It now
  raises an error saying so. A critically damped system, where the two terms are
  equal, still gives a frequency of zero.
- readAudio() returned an empty wave for a file whose data chunk size was never
  written, which is what a recorder leaves behind when it loses power part-way
  through a file, discarding the whole recording the file still held. The audio
  of such a file is now read to the end of the file, with a warning, and
  audiomothExpand() warns on a triggered recording left the same way. A file
  that really does hold no audio, such as one an AudioMoth writes when its
  battery fails as it starts to record, is still read as empty.
- readAudio() returned the audio of a wave file that had been cut short, by a
  flat battery, a full card or a copy that did not finish, without saying so.
  The C2 file of the malformed wave files reference set declares 5.67 seconds
  of audio and holds 3.40, and came back as 3.40 seconds of recording with
  nothing to tell it from one that was meant to be that long. It is now read
  with a warning classed `truncatedAudio` naming the sample frames the file
  declares and the frames it holds. A file cut short part-way through a sample
  frame fared worse: read as WAVE, the rest of the frame was made up by
  recycling the samples before it, which only a warning from `matrix()` hinted
  at, so the wave ended in a sample that was never recorded; read through the
  av package, as an RF64 file is, it was refused. Only the whole sample frames
  such a file holds are now read.
- readAudio() returned samples that were not finite numbers, which a
  floating-point file can hold, so that every measurement made from the wave
  came back as NaN. They are now read as zero, with a warning saying how many.
  Infinite samples read through the av package were already turned into zeroes
  but are now reported as well.
- generateNoise() could send samples outside the range the format holds. The
  noise was scaled by its largest positive sample rather than by its largest
  either way, so noise with a deeper trough than peak went past the level asked
  for, and an unsigned 8 bit wave holding a zero gave noise.ref="file" a
  reference of 128 where only 127 is available above the centre. Both are
  bounded now, so the result stays within the format.
- convert2seconds(), dielFraction(), yearlyFraction() and dielHistogram() read a
  time that carries no timezone of its own in the timezone of the machine they
  ran on, so the same moment fell at a different point of the day, and sometimes
  on a different day of the year, depending on where the analysis was done. A
  time carrying no timezone is now read as UTC, which is how dielPlot() draws its
  bands, so points and bands agree. These functions take a new timezone argument
  for reading a time in a stated zone instead. A time that carries a timezone,
  such as the output of parseFilename() or of suncalc, is read in that timezone
  as before, so plots built from those are unchanged.
  Note that as.POSIXct("2024-06-15 12:00") with no tz argument reads the string
  in the timezone of the machine, and so is a different moment on each machine
  before these functions ever see it. Give as.POSIXct() a tz, or pass the string
  itself, to get the same answer everywhere.
- dayPhases() and daysPhases() took the calendar day of a time in UTC rather than
  in the timezone they were given, so for a recorder far enough east or west that
  the two disagree they returned the phases of the wrong day. A recording made at
  08:00 in Sydney, which is 21:00 the day before in UTC, was given the previous
  day's phases.
- dayPhase() dropped every phase bounded by a solar event that does not occur at
  the latitude and date asked for, so a recording made during such a phase was
  reported as belonging to no phase at all and nothing said so. At the default
  location in midsummer, which is the example in ?dayPhase, there is no
  astronomical night: Night and the phases either side of it were dropped, and
  14241 of the recording's 40000 seconds, the whole of the short night, were
  unaccounted for. Further north more of them went, a summer night at 58 degrees
  coming back with the moon and no solar phase at all, and polar day returning no
  rows whatever. Each phase now runs from the event that begins it to the next
  event that does occur, which can be the following morning or, in polar day, two
  months later, so that the phases account for the whole of a recording wherever it
  was made. A phase lasting longer than a day is named for the sun's altitude
  within it rather than for the event it began at, as the sun does not stay where
  that event left it: the midnight sun begins with the sun's disc touching the
  horizon, which is the Sunset phase, and then climbs away from it for weeks, and
  is reported as Day.
- dayPhase() and dayPhases() dropped the moon's interval wherever the moon did not
  set on the day it rose or the day after, which at high latitudes it does not for
  days on end, and dayPhase() also missed moonlight that began before the days it
  gathered. Both the set that ends a night's moonlight and the rise that began it
  are now searched for across the days around the recording: at 65 degrees north
  the moon rises on 11 April 2024 and stays up until it sets on 17 April, which is
  now one interval rather than none.
- dayPhase() numbered repeated phases by the day they were found on, so a recording
  spanning two days gave Day1 and Day2 and none called Day. They are numbered among
  the rows returned instead, as Day and Day1, and the solar phases are now given
  before the moon's rows rather than interleaved with them.
- The tests of convert2seconds() and dayPhases() compared values the size of an
  epoch with the tolerance expect_equal() allows, which is relative and so comes
  to about 25 seconds at a present-day date. They passed whether or not the
  seconds were right, and now compare exactly.
- generateNoise() failed with "Float (non pcm) Wave objects must have a resolution
  of either 32 or 64 bit" for a wave that was not 32 or 64 bit, which is most of
  them, as readAudio() now gives a 16 bit wave for a 16 bit file. The noise was
  asked for at the resolution of the wave being augmented, and tuneR::noise()
  only makes floating point waves, which may only be 32 or 64 bit. The noise is
  now made at 32 bit whatever the wave is; only its samples are used, and they
  are scaled to the wave, so its resolution does not reach the result.
- generateNoise() with noise.ref="max" referenced the noise to the full scale of
  the noise rather than of the wave it is added to. The noise is floating point
  and so full scale at one, which made the noise added to a 16 bit recording
  thirty thousand times quieter than asked for.
- pythonInstall() built the `sonicscrewdriver` environment on whichever Python
  was newest, and the machine learning packages it installs lag new Python
  releases: on Python 3.14 and 3.15 numpy was built from source and failed, and
  the only tensorflow published was a release candidate, which pip passes over.
  It now asks for a Python the packages are published for, and says so plainly
  if the machine has none, rather than failing part way through pip.
- writeAudio() and .writeAudioFlac() failed with "FLAC program was not found" on
  Windows unless the flac program was in C:/Program Files/FLAC, which is the only
  place seewave::wav2flac() looks there. A flac elsewhere on the PATH, such as the
  one Rtools installs, is now passed to it, so writing FLAC works wherever the
  program is.
- audioblast() raised "must have 'max' > 'min'" for a query that matched nothing,
  unless it was called with quiet=TRUE. Such a query used to come back without a
  data field and now comes back with an empty one and a last page of zero, which
  was taken as a page of results to fetch and so as a progress bar of no length.
  A query matching nothing returns NULL, as it did before.
- readAudio() returns audio read through the av package (e.g. FLAC) at the bit
  depth of the source file, rather than at a bit depth inferred from the decoded
  samples. Audio read this way now compares equal to the same audio read from a
  WAVE file. Note that a 24bit file is returned as 32bit, as the av package does
  not report a bit depth of 24.
- readAudio() no longer leaves the sample_rate and channels attributes attached
  by av::read_audio_bin() on the samples of the Wave object it returns.
- readAudio() raised "result would be too long a vector" when given a start point
  but no end point, for every format not read by readWave() (e.g. FLAC, MP3). The
  end point defaults to Inf, which seewave::cutw() rejects as a point past the end
  of the recording. A start with no end now reads to the end of the file, as does an
  end point beyond the recording, which before this returned samples padded with NA
  when counted in samples.
- readAudio() returned only the left channel of a stereo file read as MP3 when
  the section wanted was given in seconds, minutes or hours, as seewave::cutw()
  returns a single channel per call. The same section counted in samples kept
  both channels, so the result depended on the units asked for. Both channels are
  now returned whatever the units.
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
- audiomothWave() read its fields by counting characters from fixed phrases in the
  AudioMoth comment, so the rewording in AudioMoth-Firmware-Basic 1.6.0 left gain
  empty and filter, filter.limit and cancelled FALSE for every recording made since
  May 2022. Fields are now matched by pattern, and all comment wordings from
  firmware 1.0 onwards are read. Recordings cancelled by the magnetic switch or an
  SD card write error are also now reported, as is the gain index written by
  firmware before 1.4.0.
- audiomothWave() returned a fragment of the recording timestamp as the serial
  number of recordings made with a deployment ID configured, as those files carry
  the deployment ID in place of the serial number. The deployment ID is now
  returned as deployment_id and serial is NA for such files.
- audiomothWave() returned a serial number with a trailing space, so comparing it
  against a 16 character serial number was always FALSE.
- audiomothWave() took filter.limit from the frequency trigger rather than the
  filter on recordings that used both, as it looked for the first frequency in the
  comment rather than the one in the filter clause.
- audiomothWave() returned NA for the voltage of a recording whose battery was
  below the range the firmware measures, unless it was made on firmware 1.4.0 or
  later, as it looked for that version's threshold and wording. The comparison is
  now read apart from the threshold, so the thresholds written by every version
  are returned, and the two decimal places firmware 1.0 wrote a measured voltage
  to are no longer rejected.
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
  check failed on it. It now names a file the package ships.
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
- parseFilename() returned a different instant for the same filename depending on
  the timezone of the machine it ran on. The AudioMoth, Wildlife Acoustics SM2 and
  Wildlife Acoustics SM3 formats took the wall clock in the filename to be in the
  session timezone, while the YYYYMMDD and YYYYMMDD_HHMMSS formats took it to be
  UTC, so the same recording parsed as two of its documented formats could be hours
  apart. The timezone argument now states which timezone the clock of the recorder
  was set to, for every format holding a wall clock, and defaults to "UTC". Results
  no longer depend on the timezone of the analysis machine, but datetimes for
  AudioMoth, SM2 and SM3 files will change on a machine that is not set to UTC.
- parseFilename() with format="AudioMoth" ignored its timezone argument, as
  seewave::audiomoth() does not use the timezone it is passed for the newer
  YYYYMMDD_HHMMSS filenames. These are now read by parseFilename() itself, and
  seewave::audiomoth() is still used for the hexadecimal filenames, where the
  timezone only sets how the instant is displayed.
- parseFilename() with timezone= on a Wildlife Acoustics SM2 or SM3 filename
  relabelled the timezone of an instant that had already been read in the session
  timezone, rather than reading the wall clock in the timezone asked for.
- parseFilename() gave the error "a character vector argument expected" when given
  a list of filenames together with a format other than "match".
- parseFilename() with format="match" gave the error "Unknown format: match" when
  no single format fitted every filename, and now reports what went wrong. Given a
  single filename rather than a list, it now detects the format of that file.
- parseFilename() raised an error on AudioMoth and Wildlife Acoustics filenames
  given with a path rather than a bare filename.
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
- autoBandPass() measured the centre frequency and bandwidth with a floor of 1kHz
  whatever lowcut it was given, so a band below 1kHz could never be chosen. It
  high-passed the recording at lowcut but did not pass that frequency on to
  frequencyStats(), which takes the measurement for it, so the default floor of
  that function stood however low the high-pass was set. A recording whose signal
  lies below 1kHz, as the call of a mole cricket does, came back filtered to a band
  holding nothing but numerical residue, and nothing warned. The frequency is now
  passed on, so lowcut=0 measures the whole recording as the documentation says it
  does. Results at the default lowcut of 1000Hz are unchanged, that being the floor
  which was already applied.
- the lowcut of frequencyStats() is now in Hz, as every other frequency the package
  takes is. It was in kHz, the unit of the spectrum that function measures, which
  left two arguments of the same name in neighbouring functions holding the same
  quantity in different units. The default is the same frequency as before, written
  as 1000 rather than 1, so a call that leaves it alone is unaffected. A call that
  gives it explicitly needs the value multiplied by a thousand, and applies a cut a
  thousand times lower than the one asked for until it is. The frequencies and
  bandwidths frequencyStats() returns are in kHz as they were, and both help pages
  now give the unit of every frequency they take or return.
- autoBandPass() now refuses a lowcut that is not one frequency the recording
  holds: several frequencies, an NA, text, a negative frequency, or one at or above
  half the sample rate, where the high-pass leaves nothing to measure and seewave
  reported the band it was handed as an error about its own arguments.
- autoBandPass() failed on a signal narrow enough to fall in a single bin of the
  spectrum, such as a pure tone or a stable insect call, with "'from' should be
  strictly inferior to 'to'" from seewave, which named neither the function nor the
  reason. Such a signal has no bandwidth to measure at the threshold asked for, so
  the band ran from the centre frequency to the centre frequency. A band one bin of
  the spectrum wide is kept instead, which keeps the peak rather than nothing, and
  the warning names the threshold that had no bandwidth to measure. The warnings
  frequencyStats() raises about a single bin are no longer passed on from here, as
  they are raised for both thresholds where only one is the threshold the filter is
  built from.
- autoBandPass() now refuses a number of bandwidths it cannot build a band from.
  An n.bw below 1 asks for a band of no width, or for one whose edges are the wrong
  way round, and gave the same error from seewave about its own arguments. A band
  reaching past either end of the recording, which several bandwidths either side
  of a low centre frequency do, is kept as far as the recording goes rather than
  reaching below zero, where seewave reported it as an error about subscripts.
- readAudio() took the sample rate in the header of a wave file without comment
  even where the recorder's own metadata disagreed with it, and the header can be
  the one that is wrong. A file holding 384000 in its GUANO metadata and 38400 in
  its header came back a tenth of its real rate, so the recording read as ten
  times its length and a 40kHz bat call landed at 4kHz, with nothing in the wave
  to show it. The disagreement is now reported with a warning naming both rates.
  The wave is still returned at the rate the header gives, as that is the rate its
  samples were read at, and nothing in the file says which of the two is right.
  The rates compared against the header are the `Samplerate` of the GUANO
  metadata and the `F=` rate of each generation of a bext coding history. A
  time-expanded recording is stored at a lower rate than it was made at, which is
  no fault: GUANO records the factor in its `TE` field, so a rate meeting the
  header rate multiplied by that factor is agreement, and of a coding history
  holding several generations any one of them meeting the header is enough.

## Internal
- directionPlot() named its columns with aes_string(), deprecated since ggplot2
  3.0.0, and drew the mean direction against the whole of the plot's data, which
  ggplot2 warned about on every call. Both are now written as the rest of the
  package writes them, and the plot is drawn without warnings.
- Tests were added for the parts of the package that had none: readBirdNet(),
  gs_transcribe() and the Google Speech wrappers, labelPadding() and
  labelReduction(), directionPlot(), beatSpectrum() and beatComplexity(),
  autoBandPass(), dielHistogram(), pd_simple(), ntd(), defaultCluster() and the
  retry intervals. Tests were also added for the input guards, polar-latitude
  cases and error paths of daysPhases(), writeAudio(), fcis(), package.installed(),
  pseudoWave(), audioblast(), the Annotation functions, pulseDetection(),
  pythonInstall() and dielPlot(). Nothing in them reaches the network, a paid API
  or an installer: the packages those functions call are stood in for.
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
