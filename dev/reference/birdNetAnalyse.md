# Analyse sound files using BirdNET-Analyzer

This function takes a list of sound files and analyses them using the
`BirdNET-Analyzer` (Kahl et al. 2021) . The function either returns a
data frame with the results of the analysis or a list of `Annotation`
objects.

## Usage

``` r
birdNetAnalyse(
  files,
  lat = NULL,
  lon = NULL,
  date = NULL,
  output = "Annotation"
)
```

## Arguments

- files:

  A character vector of file paths.

- lat:

  A latitude or vector of latitudes.

- lon:

  A longitude or vector of longitudes.

- date:

  A `Date` or list of `Date` objects .

- output:

  One of "data.frame" or "Annotation".

## References

Kahl S, Wood CM, Eibl M, Klinck H (2021). “BirdNET: A deep learning
solution for avian diversity monitoring.” *Ecological Informatics*,
**61**.

## Examples

``` r
if (FALSE) { # \dontrun{
  birdnetAnalyse(files=c("path/to/file1.wav", "path/to/file2.wav"), output="data.frame")
} # }
```
