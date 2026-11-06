# A S4 class for tagged waves

The TaggedWave class extends the Wave class from the tuneR package so
that a wave carries where it came from, what is known about it and a
record of what has been done to it.

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
