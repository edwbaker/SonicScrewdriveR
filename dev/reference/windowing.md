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
  ...
)
```

## Arguments

- wave:

  A Wave object or filename. Using filenames may save loading an entire
  large file into memory.

- window.length:

  The length of the analysis window (in samples).

- FUN:

  FUN to be applied to windows.

- window.overlap:

  The overlap between successive windows (in samples), a negative value
  will result in a gap between windows.

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

## Examples

``` r
if (FALSE) { # \dontrun{
windowing(wave, window.length=1000, FUN=function(w, ...) length(w), window.overlap=0)
} # }
```
