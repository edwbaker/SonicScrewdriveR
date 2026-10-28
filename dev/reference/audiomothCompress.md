# Write a Wave as an AudioMoth triggered (T.WAV) file

Writes a mono 16-bit Wave object in the AudioMoth triggered file format,
leaving out the periods that an amplitude threshold would not have
triggered a recording on. The wave is divided into segments of `segment`
bytes, and a segment whose peak amplitude is below `threshold` is
omitted and replaced by an encoded block recording its length. Omitted
audio is not recoverable: expanding the file gives silence in its place,
as it would for a recording the device had made with that threshold.

## Usage

``` r
audiomothCompress(
  wave,
  file,
  threshold,
  segment = .AM_SEGMENT_SIZE,
  comment = "",
  artist = "",
  guano = NULL,
  overwrite = FALSE
)
```

## Arguments

- wave:

  A mono 16-bit Wave object

- file:

  Path to write the T.WAV file to

- threshold:

  Peak amplitude, as a 16-bit sample value, at or above which a segment
  is kept. This is the amplitude threshold of an AudioMoth device.

- segment:

  Length of a segment in bytes. Must be a multiple of 512 and longer
  than the 488-byte header. The default of 32768 is what current
  AudioMoth firmware uses.

- comment:

  Comment string to store in the header, of the kind
  [`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
  reads back as its `audiomoth` source. The threshold is recorded after
  it, as an AudioMoth device records it, which is how
  [`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md)
  knows to expand the file.

- artist:

  Artist string to store in the header

- guano:

  GUANO metadata to store in the file, as a guano object from
  [`guano()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guano.md)
  or
  [`guanoCreate()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guanoCreate.md),
  or NULL for none

- overwrite:

  If TRUE an existing file is replaced

## Value

The path to the written file (invisibly)

## Details

The threshold is recorded in the comment the file carries, in the
wording an AudioMoth device uses, so that the file says what was done to
it. There is nothing else in a wave file to tell a triggered recording
from any other, and it is what
[`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md)
reads to know that the file needs expanding.

The first and last segments are never omitted, as the first holds the
file header and the last may be shorter than a whole segment. Where
these fall below the threshold they are written as silence, which is
what AudioMoth firmware does.

## References

Open Acoustic Devices (2021) AudioMoth Triggered T.WAV File Format.
<https://github.com/OpenAcousticDevices/Application-Notes>

## Examples

``` r
if (FALSE) { # \dontrun{
w <- readAudio(system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver"))
audiomothCompress(w, "20201103_112430T.WAV", threshold=1000)
} # }
```
