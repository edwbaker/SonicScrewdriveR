# Write an Audacity label file

Writes a list of `Annotation` objects to an Audacity label file.  
  
Internally this uses the `write.audacity()` function from the `seewave`
package (Sueur et al. 2008) .

## Usage

``` r
writeAudacityLabels(annotations, file)
```

## Arguments

- annotations:

  A list of `Annotation` objects.

- file:

  Path to the Audacity label file.

## References

Sueur J, Aubin T, Simonis C (2008). “Seewave, a free modular tool for
sound analysis and synthesis.” *Bioacoustics*, **18**(2), 213–226.
