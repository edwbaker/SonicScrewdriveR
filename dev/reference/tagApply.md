# Apply a function to a tagged wave and record the process

Runs `fun(w, ...)` and, if `w` is a `TaggedWave` or `TaggedWaveMC`,
appends a record of the call to the `processing` slot of the result. The
record holds the function name, its arguments, the time, the duration
and digests of the input and output. Untagged waves are processed
without any record being made, so `tagApply()` can be used safely in
code that handles either.

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

If `fun` returns a wave without tags, the `origin`, `metadata` and
`processing` of `w` are carried over to it. The `spectrogram_maad_power`
slot is not carried over, as it no longer matches the signal.

If `fun` returns something that is not a wave, such as a number or a
data frame, there is no tagged object to hold the record. The result is
returned with an attribute `processing` holding the history of `w`
followed by the new record (without an output digest). A `NULL` result
is returned as it is.

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
