# Apply a function to a tagged wave and record the process

Runs `fun(w, ...)` and, if `w` is a `TaggedWave` or `TaggedWaveMC`,
appends a record of the call to the `processing` slot of the result.
Untagged waves are processed without any record being made, so
`tagApply()` can be used safely in code that handles either.

## Usage

``` r
tagApply(.w, .fun, ..., label = NULL, untag = NA, cl = NULL)
```

## Arguments

- .w:

  A `Wave`, `WaveMC`, `TaggedWave` or `TaggedWaveMC` object, or a list
  of such objects.

- .fun:

  A function, or the name of one, taking the wave as its first argument.

- ...:

  Further arguments passed to `.fun`.

- label:

  Optional. A description of the process. Defaults to the name of the
  function.

- untag:

  Whether to pass `.fun` an untagged wave (see Details). The default,
  `NA`, does so for functions from other packages.

- cl:

  Optional. A cluster, as made by
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html),
  used to process a list of waves in parallel. The workers need
  `sonicscrewdriver` to be available.

## Value

The result of `.fun`, with the process recorded if `.w` is tagged.

## Details

A record is a named list holding what is needed to say what was done,
with what, when and to which audio:

- `process`:

  The label, which is the function's name unless one was given.

- `func`:

  The function as it was called, as a name or `pkg::name`.

- `versions`:

  The versions of the package the function comes from and of this
  package, as a named character vector, since the same call can give a
  different answer under another version of either. They are the
  versions loaded in the session that ran the call. A primitive is
  recorded as `base`, whose version is R's, and a function defined
  outside any package has only this package's.

- `params`:

  The arguments the caller supplied, under the names of the formals they
  filled rather than the names they were passed under, so an argument
  given by position or by an abbreviation is still named.

- `defaults`:

  The arguments the caller did not supply and the function therefore
  chose, as the function defines them. A default is part of what
  happened, and three of
  [`seewave::ffilter()`](https://rdrr.io/pkg/seewave/man/ffilter.html)'s
  twelve change the samples it returns. They are deparsed rather than
  evaluated, because a default can be an expression over the other
  arguments whose value is known only inside the call; for a literal the
  two are the same. Absent where a function has no defaults left to it,
  as the functions of this package do not, passing their arguments on in
  full.

- `time`, `timezone`:

  When it ran, and the zone the clock was keeping. The zone is named
  because a `POSIXct` alone is shown in the zone of whoever reads it,
  and loses the zone entirely once written out as text.

- `duration`:

  How long the call took, in seconds.

- `input_hash`, `output_hash`, `hash_method`:

  Digests of the samples going in and coming out, and which digest they
  are. These are what make the rest checkable: a history can be tested
  against the audio it claims to describe rather than taken on trust.

## How much the digests prove

A full digest reads every sample, which is about a second per minute of
stereo 48 kHz audio and is paid on the wave going in and the wave coming
out of every step. An hour-long recording would be minutes of hashing
per step, so the default is `"sketch"`: the length of each channel and a
fixed number of evenly spaced blocks of it, at the same cost whatever
the length.

A channel short enough that the blocks cover it is digested whole, so
for a recording of up to some seconds a sketch reads every sample. Past
that it does not, and the difference matters: a sketch will tell an
edited recording from an unedited one, but two recordings that agree on
a sketch are not thereby shown to be the same, as a change between the
blocks is not read. If a history has to be evidence rather than a good
indication, set `options(sonicscrewdriver.provenance="full")` and pay
for it.

Either way `hash_method` records which was used, so a digest is never
taken for more than it is worth. An option is not carried to the workers
of a `cl` cluster that does not inherit it, which use the default and
record that they did.

If `fun` returns a wave without tags, the `origin`, `metadata` and
`processing` of `w` are carried over to it. The `spectrogram_maad_power`
slot is not carried over, as it no longer matches the signal.

If `fun` returns something that is not a wave, such as a number or a
data frame, there is no tagged object to hold the record. The result is
returned with an attribute `processing` holding the history of `w`
followed by the new record, which is complete but for an output digest,
as no wave came out, and its `output`, which is left for whatever keeps
the result to fill in. A `NULL` result is returned as it is, as it
cannot carry an attribute.

Functions from other packages are given an untagged wave. seewave, for
example, checks the class of a wave with `class(w)[1] == "Wave"` and so
rejects a `TaggedWave`. The tags are put back on a wave that is
returned, as described above. Functions from this package and functions
defined by the user are given the tagged wave unless `untag = TRUE`.

Only the outermost call makes a record. A function that calls
`tagApply()` itself, as a function of this package may come to, is
recorded once when it is passed to `tagApply()` in turn, under the name
and arguments of the outer call rather than twice under both.

The wave and the function are named `.w` and `.fun` so that the
arguments meant for `.fun` reach it. R matches a named argument against
the arguments named before `...` by its first letters as well as in
full, so a plain `w` and `fun` would swallow anything called `f`, `fu`
or `w` - and `f` is what seewave calls a sampling frequency, which is
exactly the sort of call this function is for. `label`, `untag` and `cl`
come after `...` and so must be named in full, which leaves only a
function with an argument of one of those three names beyond reach.

## See also

[`flattenOutput()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/flattenOutput.md),
which turns the output a record holds into rows.

## Examples

``` r
tw <- tagWave(tuneR::sine(440, duration = 44100, samp.rate = 44100))
tw <- tagApply(tw, tuneR::normalize, unit = "16")
#> Warning: pcm set to TRUE since unit was one of 8, 16, or 24
tw@processing[[1]]$func
#> [1] "tuneR::normalize"

# seewave functions can be used with tagged waves
if (FALSE) { # \dontrun{
tw <- tagApply(tw, seewave::ffilter, from = 1000, to = 2000, output = "Wave")
} # }
```
