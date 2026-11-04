# Calculate what can be measured of a wave and the file it was read from

Works out the hash of the file a wave was read from and of the samples
it holds, with its length, format and size, and keeps them in the
`metadata` slot of the wave as `calculatedMetadata`, beside the
`audioMetadata` read from the file. It only calculates: nothing is
compared with what the source of a recording says of it.

## Usage

``` r
calculateMetadata(w)
```

## Arguments

- w:

  A Wave, WaveMC, TaggedWave or TaggedWaveMC object. A Wave or WaveMC
  object is tagged first.

## Value

A TaggedWave or TaggedWaveMC object holding the fields in
`w@metadata$calculatedMetadata`.

## Details

The fields are those audioBlast calculates of the recordings it holds,
under the same names without their `calculated_` prefix, with the hash
of the samples added:

- `hash`: the SHA-256 hash of the bytes of the file, in lower case, as
  audioBlast and the `hash` column of a
  [RecordingSet](https://sonicscrewdriver.ebaker.me.uk/dev/reference/recordingSet.md)
  hold it.

- `hash_method`: the digest `hash` is, `"sha256"`.

- `wave_hash`: a SHA-256 digest of every sample the wave holds, as
  [`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md)
  takes of the waves it records under
  `options(sonicscrewdriver.provenance="full")`. Two files holding the
  same audio under different tags have the same wave hash and different
  file hashes.

- `wave_hash_method`: `"full"`, as every sample is read.

- `frames`: the number of sample frames the wave holds.

- `duration`: `frames` divided by `sample_rate`, in seconds.

- `sample_rate`: in Hz.

- `channels`: the number of channels.

- `bit_depth`: the bits a sample takes in the file, or `NA` for a lossy
  format such as MP3, which stores no samples as such.

- `bit_rate`: the bits a second of audio takes in the file.

- `codec`: the format the audio is in, as FFmpeg names it, e.g.
  `"pcm_s16le"` or `"mp3"`.

- `bytes`: the size of the file.

- `at`: when they were calculated, in UTC.

- `by`: the version of sonicscrewdriver that calculated them.

The file is the one
[`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md)
read the wave from, which it records on the wave. The file is hashed
when this is called, so it should be called before the file is changed
or deleted. A wave read from part of a file has the `hash` and `bytes`
of the whole file, and the `frames` and `duration` of the part. A wave
that was not read from a file has `NA` for `hash`, `hash_method`,
`bit_rate`, `codec` and `bytes`, and its own bit depth.

For a wave file, `codec`, `bit_depth` and `bit_rate` are read from its
format chunk. For any other file they are asked of the av package where
it is installed, and are otherwise `NA`, but for the `codec` of an MP3
file.

The fields describe the file as it was read. They are carried unchanged
onto the waves
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md)
makes from the wave, and are not calculated again when a process changes
its samples, as the record of each process holds a digest of the wave it
made.

Hashing every sample takes about a second per minute of stereo 48 kHz
audio.

## Examples

``` r
f <- system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")
w <- calculateMetadata(readAudio(f))
w@metadata$calculatedMetadata$hash
#> [1] "b572a554a0b1531be3a6bc637c37d68e0fb50d7152bb492b2c7e91f5a6a86bec"
w@metadata$calculatedMetadata$duration
#> [1] 5
```
