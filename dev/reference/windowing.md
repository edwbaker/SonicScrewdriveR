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
  a length equal to window.length.

- cluster:

  A cluster form the 'parallel' package for multi-core computation.

- ...:

  Additional parameters to FUN

## Examples

``` r
if (FALSE) { # \dontrun{
windowing(wave, window.length=1000, FUN=duration, window.overlap=0, bind.wave=TRUE)
} # }
```
