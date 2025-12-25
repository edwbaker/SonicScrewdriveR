# Radar range

Calculates the distance of an object based on the round trip time of an
echolocation pulse

## Usage

``` r
radarRange(t, c = soundSpeed(medium = "air"))
```

## Arguments

- t:

  Time in seconds

- c:

  Speed of sound in transmission medium m/s (by default air)

## Value

Distance to object

## Examples

``` r
radarRange(2)
#> [1] 343
radarRange(2, c=343)
#> [1] 343
radarRange(2, c=soundSpeed(medium = "sea water"))
#> [1] 1500
```
