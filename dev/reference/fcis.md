# False-colour index spectrogram

Calculates a false-colour index spectrogram, in which three acoustic
indices are calculated for each frequency bin of each time window and
mapped to the red, green and blue channels of an image. Long recordings
can be summarised in a single image, with different sounds appearing as
different colours.

## Usage

``` r
fcis(
  x,
  window.length = NULL,
  indices = c("power", "ACI", "entropy"),
  wl = 256,
  cluster = NULL,
  scale = NULL
)
```

## Arguments

- x:

  A Wave object, or a vector of filenames which are treated as
  consecutive parts of one recording.

- window.length:

  Length of each column of the image in samples. If NULL, each file
  gives a single column.

- indices:

  The three indices to map to the red, green and blue channels.

- wl:

  Window length used for the spectrograms.

- cluster:

  A cluster from the 'parallel' package for multi-core computation. This
  is only worthwhile when x is a filename and the windows are long, as
  each worker then reads its own part of the file. Passing a Wave object
  sends a copy of it to every worker, which takes longer than the
  calculation itself.

- scale:

  NULL, a function, or a list of three functions, one per channel,
  applied to an index across the whole spectrogram before it is mapped
  to a colour.
  [`fcisClip()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/fcisClip.md)
  makes one that clips to a range of percentiles.

## Value

An object of class "fcis", being a list of the colours as a character
matrix of one entry per frequency bin and window, the matrix of values
for each index, the frequencies of the bins in Hz, the duration of a
window in seconds, and the names of the indices used. The frequencies
were in kHz, as seewave::spectro() gives them, in earlier releases.

## Details

Indices are scaled across the whole of the input, so that colours are
comparable between the windows of one spectrogram but not between
separate spectrograms. The available indices are described in
[`fcisIndexNames`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/fcisIndexNames.md).

## Examples

``` r
if (FALSE) { # \dontrun{
f <- fcis(wave, window.length=44100)
plot(f)

f <- fcis(wave, window.length=44100, indices=c("background", "ACI", "cover"))
} # }
```
