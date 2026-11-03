# Download audio files from audioBlast

Downloads audio files associated with a search using the audioBlast()
function.

## Usage

``` r
audioblastDownload(
  d,
  metadata = TRUE,
  skip.existing = TRUE,
  dir = ".",
  quiet = FALSE,
  on.issue = .audioblastIssue
)
```

## Arguments

- d:

  Data returned from a search using audioBlast(), as a data frame or a
  `RecordingSet`.

- metadata:

  If true saves the data in d as a csv file.

- skip.existing:

  If true will not overwrite existing files.

- dir:

  Directory to save files to.

- quiet:

  If true will not print progress.

- on.issue:

  Function to call on error or warning. By default `stop` to raise a
  standard R error. Setting to `warning` will instead a warning.

## Value

The paths of the files, invisibly.

## Details

Where audioBlast has measured a file, the SHA-256 hash of the file
downloaded is checked against the hash audioBlast measured, and a file
that does not match is reported through `on.issue`.

A
[RecordingSet](https://sonicscrewdriver.ebaker.me.uk/dev/reference/recordingSet.md)
is downloaded from the URL each recording has as its `location`, and
checked against its `hash`.
