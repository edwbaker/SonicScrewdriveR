# Expand an AudioMoth triggered (T.WAV) file

AudioMoth devices recording on a trigger, such as an amplitude
threshold, write a single T.WAV file in which the periods that did not
trigger a recording are left out. Each omitted period is replaced by a
512-byte encoded block recording its length, so that the original timing
of the triggered recordings can be restored. This function restores
them, returning either a Wave object or an ordinary WAV file in which
the omitted periods are silence.

## Usage

``` r
audiomothExpand(file, output = NULL, overwrite = FALSE)
```

## Arguments

- file:

  Path to the T.WAV file to expand

- output:

  Path to write the expanded WAV file to. If NULL (the default) no file
  is written and a Wave object is returned instead.

- overwrite:

  If TRUE an existing output file is replaced

## Value

A Wave object, or, if `output` is given, the path to the written file
(invisibly)

## Details

Only mono 16-bit PCM files are supported, which is what AudioMoth
devices produce. A file holding no encoded blocks is returned unchanged,
so this is safe to call on any AudioMoth recording. Metadata chunks,
including the GUANO chunk written by current firmware, are carried over
to the expanded file, and are attached to a returned Wave object as a
"guano" attribute, which
[`guano()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guano.md)
will read.

Writing to `output` streams the expanded file to disk, so a recording
that expands to more than memory can hold can still be expanded.
Returning a Wave object necessarily holds the whole expanded recording
in memory.

A recording cut short by a recorder that lost power is reported with a
warning classed `truncatedAudio`, or `unwrittenAudioSize` where the size
of the audio was never written, and the audio that is there is expanded.
A recording in the RF64 or BW64 form whose ds64 chunk miscounts its
sample frames is reported with a warning classed `frameCountMismatch`,
and the count is put right in the file written. All of them are also
classed `audioFileWarning`; see
[`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md)
for the warnings that share those classes and for handling one of them
without silencing the rest.

## References

Open Acoustic Devices (2021) AudioMoth Triggered T.WAV File Format.
<https://github.com/OpenAcousticDevices/Application-Notes>

## Examples

``` r
if (FALSE) { # \dontrun{
# Read a triggered recording into a Wave object
w <- audiomothExpand("20201103_112430T.WAV")

# Expand every triggered recording on a card, as the reference tool does
for (f in list.files(pattern="T\\.WAV$")) {
  audiomothExpand(f, sub("T\\.WAV$", ".WAV", f))
}
} # }
```
