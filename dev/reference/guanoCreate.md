# Create GUANO metadata

Builds a guano object from named fields. Field names follow the GUANO
specification, so a name may carry a namespace, as in "OAD\|Battery
Voltage", and the specification requires that any field it does not
define is namespaced.

## Usage

``` r
guanoCreate(..., version = "1.0")
```

## Arguments

- ...:

  Named fields. A name containing a space or a vertical bar must be
  quoted. A GUANO line ends its name at the first colon, so the
  whitespace around a name is no part of it and is dropped here, leaving
  the name the field will have when the file is read back. Two names
  alike but for that whitespace are therefore one name, and refused as a
  repeat.

- version:

  The GUANO version to record

## Value

A guano object, which is a named list of fields

## References

Riggs, D. GUANO: Grand Unified Acoustic Notation Ontology.
<https://github.com/riggsd/guano-spec>

## Examples

``` r
guanoCreate(
  Make = "Open Acoustic Devices",
  Model = "AudioMoth",
  Samplerate = 48000,
  "Loc Position" = c(51.4964, -0.1224),
  "Species Manual ID" = c("Pipistrellus pipistrellus", "Myotis daubentonii")
)
#> GUANO|Version     1.0
#> Make              Open Acoustic Devices
#> Model             AudioMoth
#> Samplerate        48000
#> Loc Position      51.4964 -0.1224
#> Species Manual ID Pipistrellus pipistrellus,Myotis daubentonii
```
