# Reproduction fixtures for codereview/metadata-formats.md

Each script builds its input bytes with `writeBin`/`charToRaw` and runs the
package's own parser over them. They are the evidence behind the CONFIRMED
findings in `../metadata-formats.md`, kept because the bytes are not
reconstructible from the prose.

They source the package's R files directly rather than loading the package, so
they need no `tuneR` and no installed build:

```bash
Rscript codereview/fixtures/pad-byte-misalignment.R
Rscript codereview/fixtures/id3-utf16-encoding.R
Rscript codereview/fixtures/flac-audiomoth-comment.R
```

`flac-audiomoth-comment.R` reads `inst/extdata/AUDIOMOTH.flac` and
`inst/extdata/AUDIOMOTH.mp3`, so run it from the repository root.

This directory is review material, not package source; it is not meant to be
merged into `master` as it stands.
