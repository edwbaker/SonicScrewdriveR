# Run a pipeline over a set of recordings

Runs a
[`pipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pipeline.md)
on every file of a
[RecordingSet](https://sonicscrewdriver.ebaker.me.uk/dev/reference/recordingSet.md)
and writes its results to a folder, the store. The store is the whole
state of a run, so the same call made again carries on where the last
one stopped, and the results are read back with
[`pipelineResults()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pipelineResults.md).

## Usage

``` r
runPipeline(
  p,
  set,
  store,
  batch = 1000,
  max.memory = 4e+09,
  keep = NULL,
  retry = FALSE,
  cl = NULL,
  quiet = FALSE
)
```

## Arguments

- p:

  A `Pipeline`.

- set:

  A `RecordingSet`.

- store:

  The folder to write the results to. It is made if it does not exist.

- batch:

  How many files to run between writes to the store.

- max.memory:

  The most memory, in bytes, a file may take once decoded to be read
  whole, 4 GB unless it is given, or `Inf` to read every file whole.

- keep:

  A folder to keep downloaded files in, or `NULL` to delete each one
  once it has been run. Each file is kept in a folder of its own, named
  from its URL, so that files of the same name from different places are
  kept apart, and one already there is not downloaded again.

- retry:

  If `TRUE`, files that failed in an earlier run are run again.

- cl:

  A cluster from the parallel package, to run batches on, or `NULL`.

- quiet:

  If `TRUE`, no progress is reported.

## Value

The set, invisibly, with the hash of every file the store holds filled
in where the set gave none, so that its rows can be joined to the
results by `hash`.

## Files

The recordings of a set that share a hash or a location are one file, as
a recording the Animal Sound Archive lists under three species is, and
each file is run once. A file is fetched from its location: a local file
is read where it is, and one at a URL is downloaded, then deleted once
it has been run unless `keep` names a folder to keep it in. Each file is
read by
[`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md),
whole unless it is too long to hold (see Long files), tagged with its
location as its origin and with the columns of the set that all of its
recordings agree on, such as `start`, `lat` and `lon`, in the
`"recordingSet"` entry of its metadata, and measured by
[`calculateMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/calculateMetadata.md).
Columns its recordings disagree on, such as the `taxon` of a file listed
under three species, are left in the set.

A file is known by the SHA-256 hash of its bytes. A file the set gives a
hash has to match it, and a local file the set gives none is hashed
before the run starts, so that a file already in the store is recognised
wherever it has been moved to, and copies of one file are run once. A
file that cannot be hashed then is left to fail when it is read. A file
at a URL the set gives no hash is recognised by its location once it has
been run. A local location is made absolute before the run, as a worker
of a cluster can be working in another folder.

## Long files

A file is read whole unless it would take more than `max.memory` bytes
once decoded, which is worked out from its header before it is read. R
holds a sample in 4 bytes for a RIFF WAV file of 8, 16 or 32-bit PCM,
for an AudioMoth triggered recording, and for an MP3 file, and in 8 for
a WAV file of 24-bit PCM or floating-point audio and for anything the av
package decodes, a WAV file in the RF64 or BW64 form and FLAC among
them. A triggered recording is measured as it will be once expanded.

A file over `max.memory` is read a window at a time, by each
[`windowing()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/windowing.md)
at the top of the pipeline, as windowing() reads a file. Each window
holds the samples, and the tags, of the window cut from the whole
recording, so the results are those a whole read gives, and the
`wave_hash` of the file is taken a block at a time. That writes the
samples to a temporary file, as the digest is of all of them at once, so
it needs room on disk for 8 bytes a sample while it is taken, and for a
file of more than two channels, whose channels are written apart and
then joined, 8 bytes more for each sample of one channel. Only a RIFF
WAV file of PCM or floating-point audio can be read in parts that are
exact, so any other file over `max.memory`, an MP3 or a FLAC file among
them, fails without being read, as does a triggered recording, which is
expanded whole. So does a file whose pipeline has a step outside a
windowing given `then=`, which needs the whole recording at once, or
windows that are themselves over `max.memory`. The size of a file other
than a WAV file is asked of the av package, and where that is not
installed such a file is read whole.

## Batches

The files not yet in the store are run in batches of `batch` files, and
each batch is written to the store once all of its files have been run.
Stopping a run loses only the batch it was in, and the next run starts
again from that batch. With `cl`, a cluster from the parallel package,
each worker runs whole batches and writes them itself, and needs this
package installed.

## Failures

A file that cannot be fetched or read, that does not match the hash the
set gives it, or on which a step fails, is recorded as failed with the
reason, and the run moves on. It has a row in every table of results,
with `error` saying what went wrong, and it is not run again unless
`retry` is `TRUE`.

## The store

The store holds a folder for each pipeline run into it, named by the
pipeline's id, so pipelines that share a store are kept apart and a file
is run once for each of them. That folder holds the pipeline itself, in
`pipeline.rds`, and a `batches` folder with a list of the files each
batch covered and a file for each of its tables of results, so that one
table can be read without the others. As the id covers the code of every
package a step uses, a pipeline run after any of them has changed is run
into a folder of its own.

## See also

[`pipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pipeline.md)
to write one, and
[`pipelineResults()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pipelineResults.md)
to read what a run wrote.

## Examples

``` r
if (FALSE) { # \dontrun{
# Working out a pipeline's id hashes the code of every package it uses,
# which takes some seconds.
f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
set <- recordingSet(f, source="AudioMoth")
p <- pipeline(windowing(1, time.unit="seconds", then=dutyCycle()))
store <- file.path(tempdir(), "store")
set <- runPipeline(p, set, store)
res <- pipelineResults(store, p)
head(res[[1]])
merge(res[[1]], set, by.x="file", by.y="hash")[, c("window_start", "value", "uri")]
} # }
```
