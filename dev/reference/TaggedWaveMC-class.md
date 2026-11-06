# A S4 class for tagged multi-channel waves

The TaggedWaveMC class extends the WaveMC class from the tuneR package
as the TaggedWave class extends the Wave class.

## Slots

- `origin`:

  Where the wave came from, such as the file it was read from, or
  "user".

- `metadata`:

  A list of what is known about the wave, such as the metadata of the
  file it was read from.

- `processing`:

  A list holding a record of each process applied to the wave, in the
  order they were applied, as described in
  [`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md).
