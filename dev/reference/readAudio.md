# Read an audio file

This file is used to read an audio file and return a Wave object, it is
an abstraction function for various specific audio reading functions. If
no existing method can be identified an attempt is made to use the av
package to read the audio.

## Usage

``` r
readAudio(
  file,
  mime = "auto",
  from = 0,
  to = Inf,
  input.unit = "seconds",
  units
)
```

## Arguments

- file:

  File to read

- mime:

  MIME type of file to read, or "auto". Supported types are
  "audio/x-wav" and "audio/mpeg" (MP3)

- from:

  Start point in file to return

- to:

  End point in file to return

- input.unit:

  One of "samples", "seconds", "minutes", "hours". Default is "seconds".

- units:

  Deprecated, use input.unit instead.

## Value

A Wave object, or a WaveMC object for a file with more than two
channels.

## Details

Mono and stereo files are returned as a
[`tuneR::Wave`](https://rdrr.io/pkg/tuneR/man/Wave.html) object, and
files with more than two channels as a
[`tuneR::WaveMC`](https://rdrr.io/pkg/tuneR/man/WaveMC.html) object
holding one column per channel, in the order the file holds them.
[`writeAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/writeAudio.md)
writes either back out, and functions that take multichannel audio, such
as
[`tdoa()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tdoa.md),
take the result directly.

WAVE files are read by
[`tuneR::readWave()`](https://rdrr.io/pkg/tuneR/man/readWave.html)
whatever their channel count, so a multichannel WAVE keeps the channel
names the file itself records. Everything read through the av package is
named by channel position instead (see
[`tuneR::MCnames`](https://rdrr.io/pkg/tuneR/man/MCnames.html)), because
the av package reports a channel layout that does not identify the
channels of the file.

Files read through the av package (which is everything that is not WAVE
or MP3, e.g. FLAC) are returned at the bit depth the file was stored at,
so that the same audio compares equal however it was read. The av
package does not report a bit depth of 24, and returns 24bit audio as
32bit, so the samples of a 24bit file are 256 times those
[`tuneR::readWave()`](https://rdrr.io/pkg/tuneR/man/readWave.html) would
give for the same audio stored as WAVE. Audio stored as floating point
is returned as 32bit.

A recording made on a trigger is expanded by
[`audiomothExpand()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothExpand.md)
as it is read, so that the periods that did not trigger a recording are
returned as silence and the timing of the triggered recordings is
preserved. Such a recording is recognised by the trigger AudioMoth
records in the comment it writes into the header, rather than by the T
before its extension: a name is only a convention, which an ordinary
recording can meet and a triggered one can be renamed out of. A file
whose comment has been lost can still be expanded by calling
[`audiomothExpand()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothExpand.md)
on it.

A recorder that loses power part-way through a WAV file can stop before
writing the size of the audio, leaving a file that declares an empty
recording while still holding the audio it was recording at the time.
The audio of such a file is read to the end of the file, with a warning,
rather than returned as nothing. A file that really holds no audio is
read as empty.

Samples that are not finite numbers, which a floating-point file can
hold, are read as zero with a warning, as they would otherwise spread
through every measurement made from the wave.

### A header that disagrees with the recorder's own metadata

A recorder can write a sample rate into its metadata that disagrees with
the one in the format chunk of the header, and the header can be the one
that is wrong. The wave is returned at the rate the header gives, as
that is the rate the samples were read at, and the disagreement is
reported with a warning naming both rates: nothing in the file says
which of them is right, and a recording read at a tenth of the rate it
was made at reads as ten times its length, with every frequency measured
from it wrong by the same factor and nothing in the wave to show it.

The rates compared against the header are the `Samplerate` of the GUANO
metadata and the `F=` rate of each generation of a bext coding history.
A time-expanded recording is stored at a lower rate than it was made at,
which is no fault: GUANO records the factor in its `TE` field, so a rate
that meets the header rate multiplied by that factor is agreement. Of a
coding history holding several generations, any one of them meeting the
header is enough, as the others describe the recording as it was before
it was last written.

### Handling these warnings

Each warning above carries a class of its own, so that one of them can
be handled without silencing the rest: `sampleRateMismatch`,
`unwrittenAudioSize` and `nonFiniteSamples`, and `truncatedAudio` from
[`audiomothExpand()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothExpand.md).
All of them are also classed `audioFileWarning`, for handling every
warning about the file at once, and `warning`, so
[`suppressWarnings()`](https://rdrr.io/r/base/warning.html) and an
ordinary warning handler treat them as the warnings they are.

    withCallingHandlers(
      readAudio(file),
      sampleRateMismatch = function(cond) invokeRestart("muffleWarning")
    )
