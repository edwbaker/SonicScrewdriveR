# Write an audio file

This function writes a `Wave` or `WaveMC` object to a file, it is an
abstraction function for various specific audio writing functions and is
the counterpart of
[`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md).
WAVE files are written directly, other formats are written by converting
a temporary WAVE file using the `av` package. If the `av` package is not
available FLAC files can also be written using the `flac` program via
the `seewave` package (Sueur et al. 2008) .

## Usage

``` r
writeAudio(wave, file, mime = "auto", ...)
```

## Arguments

- wave:

  A Wave or WaveMC object to write. The channels of a WaveMC object must
  be named, as the name is what records the position of each channel in
  the file (see
  [`tuneR::MCnames`](https://rdrr.io/pkg/tuneR/man/MCnames.html)).
  Tagged waves (see
  [`tagWave()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagWave.md))
  are written as the audio they contain, the tags are not saved.

- file:

  File to write.

- mime:

  MIME type of the file to write, or "auto" (the default) to determine
  it from the file extension (see Details).

- ...:

  Additional arguments passed to the function performing the write:
  [`tuneR::writeWave()`](https://rdrr.io/pkg/tuneR/man/writeWave.html)
  for WAVE files, and
  [`av::av_audio_convert()`](https://docs.ropensci.org/av/reference/encoding.html)
  (e.g. `bit_rate`) for other formats.

## Value

The path of the file written, invisibly.

## Details

The MIME type of the file to write is determined from the file
extension, unless it is given in the `mime` argument. Any format
supported by the encoders available to the `av` package can be written,
commonly used types include:

- **audio/x-wav** (`.wav`) - written by
  [`tuneR::writeWave()`](https://rdrr.io/pkg/tuneR/man/writeWave.html).

- **audio/flac** (`.flac`)

- **audio/mpeg** (`.mp3`)

- **audio/mp4** (`.m4a`)

Which other formats can be written depends on the encoders available to
the `av` package, and some encoders accept only certain sample rates
(e.g. Opus requires 48kHz or one of its divisors).

Audio written to a lossy format (e.g. MP3) will not be identical to the
audio held in the object, and audio written to a format that does not
support the bit depth or channel count of the object will be converted
by the encoder. Some encoders refuse a channel count they cannot hold
rather than converting it: MP3 will not take more than two channels, for
example, and reports an error from the encoder.

A WaveMC read by
[`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md)
can be written back out directly. Where it was read through the av
package its channels are named by position rather than from the file, so
the names record the channel order and not what was recorded on each
channel.

## References

Sueur J, Aubin T, Simonis C (2008). “Seewave, a free modular tool for
sound analysis and synthesis.” *Bioacoustics*, **18**(2), 213–226.

## Examples

``` r
w <- tuneR::sine(440, duration=1000)
f <- tempfile(fileext=".wav")
writeAudio(w, f)
file.remove(f)
#> [1] TRUE

if (FALSE) { # \dontrun{
# Requires the av package
writeAudio(w, "tone.mp3")
writeAudio(w, "tone.flac")
} # }
```
