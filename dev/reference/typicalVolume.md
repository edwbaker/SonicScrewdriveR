# Typical volumes

Typical volumes of everyday things.

## Usage

``` r
typicalVolume(thing = NA_character_)
```

## Arguments

- thing:

  Volume of thing, if missing then returns all volumes

## Value

Typical volume of thing in dBA, or if no thing parameter a data frame of
all volumes

## Examples

``` r
typicalVolume()
#>                        thing dBA
#> 1               steam engine  85
#> 2             printing works  87
#> 3  diesel electric generator  96
#> 4      screw-heading machine 101
#> 5               weaving shed 104
#> 6            sawmill chipper 105
#> 7          metalwork grinder 106
#> 8       wood-planing machine 108
#> 9                  metal saw 110
#> 10                 rock band 115
#> 11              boiler works 118
#> 12           metal hammering 118
#> 13              jet take-off 120
#> 14             rocket launch 160
typicalVolume("rocket launch")
#> [1] 160
```
