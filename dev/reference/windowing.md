# Windowing Function for Wave Objects

Separates a Wave object into windows of a defined length and runs a
function on the window section. Windows may overlap, and the function
can make use of 'parallel' package for multi-core processing. It will
also show a progress bar if the 'pbapply' package is installed.

## Usage

``` r
windowing(
  wave,
  window.length = 1000,
  FUN,
  window.overlap = 0,
  bind.wave = FALSE,
  complete.windows = TRUE,
  cluster = NULL,
  ...,
  time.unit = "samples"
)
```

## Arguments

- wave:

  A Wave object or filename. Using filenames may save loading an entire
  large file into memory.

- window.length:

  The length of the analysis window, in `time.unit`.

- FUN:

  FUN to be applied to windows.

- window.overlap:

  The overlap between successive windows, in `time.unit`. A negative
  value will result in a gap between windows.

- bind.wave:

  If TRUE and FUN returns wave objects, then these are combined into a
  single object

- complete.windows:

  If TRUE (default) the final window will not be processed unless it has
  a length equal to window.length. If FALSE it is processed holding what
  is left of the audio, which can be shorter.

- cluster:

  A cluster form the 'parallel' package for multi-core computation.

- ...:

  Additional parameters to FUN

- time.unit:

  The unit `window.length` and `window.overlap` are given in: one of
  "samples", "seconds", "minutes" or "hours". It comes after `...`, so
  that an argument meant for `FUN` is not taken for it, and has to be
  named in full.

## Value

A list of the results of FUN for each window, or a single Wave object if
bind.wave is TRUE.

## Details

A file is read a window at a time by
[`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md),
so its windows are placed over the audio the file holds, which is what
readAudio() reads. A recording cut short, by a flat battery, a full card
or a copy that did not finish, declares more audio than it holds, and
its windows stop where its audio does: with `complete.windows=TRUE` the
last window is the last one its audio fills, and with
`complete.windows=FALSE` a final, shorter window holds what is left of
it, as for a Wave object holding the same audio. A wave file whose audio
size was never written is windowed over the audio readAudio() recovers
from it, and a file or Wave object holding no audio gives no windows.

The length of a window and the overlap between windows are both given in
`time.unit`, which is samples unless it says otherwise. A set of
recordings mixes sample rates, and a length in samples is a different
length of time in each of them, so `time.unit = "seconds"` gives every
recording windows of the same length of time. A length in time is
rounded to the nearest whole sample, as
[`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md)
rounds a time it is given. `FUN` is given the start and length of its
window in samples, whatever `time.unit` is, as samples are where a
window falls.

## Windows in a pipeline

In a
[`pipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pipeline.md),
`then=` fills `FUN`: the steps it gives are run on each window, and
giving both `then=` and `FUN` is an error. Each window is a tagged wave
whose history is that of the wave it was cut from, followed by the cut,
so a result from a window traces back to where in the recording it was
measured. The results of each step have a row for each window, keyed by
`window_start`, `window_end` and `window_length`, in seconds from the
start of the recording, counting any cut made before the windowing by
[`cutws()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/cutws.md)
or seewave's `cutw()`. A cut made by any other function is not counted,
and the windows after it are placed as though it had not been made. A
windowing inside another adds columns of its own, numbered by how deep
it is: `window_start_2` and so on. A wave shorter than one window gives
no windows, which is an error naming the step.

## The order of windowing and filtering

Windowing then filtering does not give the numbers filtering then
windowing does.
[`filterWave()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/filterWave.md)
scales what it gives back to the peak level of the wave it was given.
Filtered before windowing, the whole recording is scaled by one factor,
so the level of a band can be compared between windows, but the whole
recording has to be in memory. Filtered after windowing, each window is
scaled by a factor of its own, raising the band in every window to that
window's own peak, so the levels of a band compared across windows
mislead, but the recording can be read a window at a time. Both orders
are sound, and the history of every window records which was used.

## Examples

``` r
if (FALSE) { # \dontrun{
windowing(wave, window.length=1000, FUN=function(w, ...) length(w), window.overlap=0)
windowing(wave, 60, FUN=function(w, ...) seewave::H(w), time.unit="seconds")
} # }
```
