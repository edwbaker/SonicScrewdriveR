# Write an analysis as a pipeline of steps

A `Pipeline` is an analysis written down as steps, to be run on every
recording in a
[RecordingSet](https://sonicscrewdriver.ebaker.me.uk/dev/reference/recordingSet.md).
Each step is written as an ordinary call to a function with the wave
left out, and the wave is put in as its first argument when the step
runs, as
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md)
does.

## Usage

``` r
pipeline(...)

task(.fun, ..., then = NULL)
```

## Arguments

- ...:

  For `pipeline()`, the steps: calls written with the wave left out,
  pipelines, or lists of pipelines. For `task()`, the arguments of
  `.fun` other than the wave.

- .fun:

  A function, or the name of one, taking the wave as its first argument.

- then:

  Optional. A pipeline, or a single step, to give the wave the step
  gives back, as described in Details.

## Value

A `Pipeline`. `task()` gives a pipeline of one step.

## Writing a pipeline

`pipeline()` keeps each call as it was written, without running it, so
that printing a pipeline shows what was typed. The arguments of each
call are evaluated once, when the pipeline is made:
`bandpass(2000, 11000)` becomes its `WaveFilter` then, so a pipeline is
fixed and can be identified, and a variable changed afterwards does not
change it.

A pipeline given as a step is run in its place, so `pipeline(p1, p2)`
runs the steps of both in turn, and is the same pipeline, with the same
id, as one written out with all of their steps. A pipeline is therefore
a step, and a step on its own is a pipeline of one: either can go
wherever the other can.

`task()` makes the same step as a call written in `pipeline()` would, as
a value, so that a pipeline can be built in code. A call written inside
[`lapply()`](https://rdrr.io/r/base/lapply.html) would run there and
then, where `task()` keeps it to run later:
`pipeline(lapply(c(1000, 4000), function(f) task(filterWave, bandpass(f, 2 * f))))`.
Its function is named `.fun` so that an argument called `f`, as seewave
calls a sampling frequency, reaches the function rather than being taken
for it, as with
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md).

An argument of `pipeline()` that is a call is a step, unless it is a
call to `pipeline()` or `task()`, or to one of the functions of base R
that build a list, such as [`list()`](https://rdrr.io/r/base/list.html)
and [`lapply()`](https://rdrr.io/r/base/lapply.html), which take no
wave. A pipeline held in a variable, or a list of them, is given by its
name, and `NULL` gives no steps.

A pipeline does not carry the recordings it is run on. They are given to
[`runPipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/runPipeline.md)
when it is run, so that the same pipeline can be run on any number of
sets, and its id is the same whichever set it runs on.

## How waves and results pass between steps

What a step does to the pipeline depends on what it gives back, which is
known only once it has run:

- A wave:

  The steps after it get the new wave, so a chain of transformations
  stays flat: `pipeline(normalise(), dutyCycle())` measures the
  normalised wave.

- Several waves:

  An error, as there is no single wave to carry on with, unless the step
  is a windowing given `then=`, as below.

- Anything else:

  The step is an analysis, and the wave it was given goes on to the next
  step as it was.

`then=` gives a step a pipeline, or a single step, of its own. The wave
the step gives goes to that pipeline, and the steps after it get the
wave as it was before it, so
`filterWave(bandpass(2000, 11000), then = dutyCycle())` measures the
band and leaves the rest of the pipeline the whole recording. `then=` is
taken out of the call before the function is called, so a function with
an argument of its own called `then` cannot be given it through a
pipeline. A step given `then=` that turns out not to give a wave is an
error, raised when it runs, and so is one that gives several.

[`windowing()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/windowing.md)
is the step that gives several waves, and takes its `then=` itself as
its `FUN`:
`windowing(60, time.unit = "seconds", then = pipeline(dutyCycle(), seewave::M()))`
runs both steps on every minute of the recording, and their tables have
a row for each window, keyed by where it starts and how long it is. A
windowing in a pipeline needs `then=`, and cannot be given both `then=`
and `FUN`.

A step of
[`cutws()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/cutws.md)
or seewave's `cutw()` is a cut, and is placed in the recording. Every
row measured on what it cuts, by the steps of its `then=` or by the
steps after it, has `cut_start` and `cut_end`, in seconds from the start
of the recording; a cut of a cut gives the span that was measured. A
window inside a cut is counted from the start of the recording too, so
`cutws(160001, 240000, then = windowing(8000, then = dutyCycle()))` on
audio sampled at 8 kHz keys its rows with a cut from 20 to 30 seconds
and windows starting at 20, 21, 22 seconds and so on. A function that
cuts in some other way, one of the user's among them, is not recognised,
and what it gives is taken to start where the wave it was given did.

The results of a pipeline are a table for each analysis step, its result
turned into rows by
[`flattenOutput()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/flattenOutput.md),
named by the step's path through the pipeline: its own call after the
calls of the steps whose `then=` it is in, joined by a slash, as
`filterWave(bandpass(2000, 11000)) / dutyCycle()`. Two steps can have
the same path, as the same analysis before and after a transformation
do, and each is then followed by its number in the outline, as
`dutyCycle() [3]`.

A function of this package that records itself, as
[`filterWave()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/filterWave.md)
and
[`normalise()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/normalise.md)
do, leaves the record it makes when called directly in the history of
the wave, and any other step is recorded as
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md)
records it. For an analysis, Pipeline puts the result in the `output` of
its record, sets the record's `output_hash` to its `input_hash`, as the
wave carried on is the wave that went in, and adds the record to the
history of the wave, so that every result traces back to what was done
to the wave before it.

## Identity

A pipeline is identified by a hash of its steps in order, so two
pipelines that would compute the same thing share an id, and changing
anything that could change a number gives a new one. A step's part of it
covers its function, its evaluated arguments, named as the function
names them so that one given by position counts the same as one given by
name, the defaults it was not given, and the code of every package it
uses. A step with `then=` adds the id of its own pipeline, which is the
same whether that pipeline is run on its own or inside another.

Functions are identified by their code, never as R objects. A function
object changes when R byte-compiles it, and carries its environment and
the source it was read from, so it would give the same pipeline a new id
in every session. A function from a package is identified by its package
and name, with the code of that package. Any other function, written by
the user or inline, is identified by its own code, so a change to
another function it calls is not seen, and nor are values a function
made by another function holds from where it was made: give those as
arguments instead.

The packages a step uses are found from its code. From its function, and
any function among its arguments such as the one in a `WaveFilter`,
calls are followed through the functions of this package and of the
user, and every other package they reach is counted, though calls within
it are not followed. This package is always counted, as its own code
runs around every step, and scikit-maad is counted wherever a step
reaches
[`getMaad()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/getMaad.md).
A package is hashed from the deparsed code of every function in its
namespace, and scikit-maad from its installed source files, so any
change to a package a step uses gives it a new id, even a change to a
function the step never calls, and so does a version that was never
released. Packages that come with R are covered by R's version, which is
part of every id. As the packages are found from the code rather than
from what ran, a package reached only on a path a step does not take,
such as one used to install another, still counts, and one that is not
installed is counted as not installed.

Code is hashed each time an id is worked out rather than remembered, so
a package changed during a session is seen the next time one is. An
argument holding an environment, such as a Python module, cannot be
identified, and gives an error.

## Printing

Printing a pipeline gives the start of its id, its steps as an outline
numbered so that a table of results can be found in it, each call as it
was written, and a last line of the code it was identified by: the start
of the hash of each package, and R's version.

## See also

[`runPipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/runPipeline.md)
to run one over a set of recordings,
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md),
whose records the steps leave, and
[`flattenOutput()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/flattenOutput.md),
which turns their results into rows.

## Examples

``` r
p <- pipeline(
  normalise(),
  filterWave(bandpass(2000, 11000), then = dutyCycle()),
  dutyCycle()
)
p
#> <Pipeline be312093: 4 steps>
#> 1. normalise()
#> 2. filterWave(bandpass(2000, 11000)), then
#>    2.1 dutyCycle()
#> 3. dutyCycle()
#> Code: sonicscrewdriver ad28d4dc, digest f0d5de56, seewave 76dcf15f, tuneR 3763cca6, R 4.6.2 (2026-10-29)

# Two bands, built in code
pipeline(lapply(c(1000, 4000), function(f) {
  task(filterWave, bandpass(f, 2 * f), then = dutyCycle())
}))
#> <Pipeline 7d12130b: 4 steps>
#> 1. filterWave(bandpass(1000, 2 * 1000)), then
#>    1.1 dutyCycle()
#> 2. filterWave(bandpass(4000, 2 * 4000)), then
#>    2.1 dutyCycle()
#> Code: sonicscrewdriver ad28d4dc, digest f0d5de56, seewave 76dcf15f, tuneR 3763cca6, R 4.6.2 (2026-10-29)
```
