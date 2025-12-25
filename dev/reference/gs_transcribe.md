# Google Speech API Transcribe

Wrapper around various Google packages to simplify speech transcription.

## Usage

``` r
gs_transcribe(filename, bucket = NULL, ...)
```

## Arguments

- filename:

  Path to file for analysis

- bucket:

  Storage bucket on Google Cloud for larger files

- ...:

  Additional arguments to pass to gl_speech()

## Value

A gs_transcribe object containing details of the transcription

## Examples

``` r
if (FALSE) { # \dontrun{
gs_transcribe("demo.wav")
} # }
```
