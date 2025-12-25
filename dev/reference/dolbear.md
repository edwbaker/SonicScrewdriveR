# Dolbear's law

Dolbear (1897) was the first to publish a formula for how the rate of
chirping of crickets varies with temperature, using the tree cricket
*Oecanthus fultoni*.

## Usage

``` r
dolbear(n = NULL, t = NULL, species = "Oecanthus fultoni")
```

## Arguments

- n:

  Chirps per minute

- t:

  Temperature in Celsius

- species:

  Species to use (by default *Oecanthus fultoni*), NULL to calculate for
  all species.

## Value

Data frame with t and n calculated for matching species.

## Details

Subsequent research by Dolbear and others have published additional
formulae for other species: *Oecanthus fultoni* (Fulton 1925) ,
*Oecanthus rileyi* (Walker 1962) , *Oecanthus alexanderi* (Walker and
Collins 2010) , and *Oecanthus allardi* (Allard 1957) .

## References

Allard HA (1957). “The stridulations of some crickets in the Dominican
Republic.” *Journal Washington Academy of Sciences*, **47**, 150–152.  
  
Dolbear AE (1897). “The Cricket as a Thermometer.” *The American
Naturalist*, **31**(371), 970–971. ISSN 00030147, 15375323,
<http://www.jstor.org/stable/2453256>.  
  
Fulton BB (1925). “Physiological variation in the snowy tree cricket,
Oecanthus fultoni, De Geer.” *Annals of the Entomological Society of
America*, **18**, 363–383.  
  
Walker TJ (1962). “The taxonomy and calling songs of United States tree
crickets (Orthoptera: Gryllidae: Oecanthinae). I. The genus Neoxabea and
the niveus and varicornis groups of the genus Oecanthus.” *Annals
Entomological Society of America*, **55**, 303–322.
<http://entnemdept.ifas.ufl.edu/walker/Buzz/s576lw62.pdf>.  
  
Walker TJ, Collins N (2010). “New World thermometer crickets: the
Oecanthus rileyi species group and a new species from North America.”
*Journal of Orthoptera Research*, **19**(2). ISSN 10826467, 19372426,
<http://www.jstor.org/stable/25822725>.

## Examples

``` r
dolbear(n=6)
#>             species location      m      c min max       source        t n
#> 1 Oecanthus fultoni     Iowa 7.7879 -30.21  15  25 Fulton, 1925 4.649520 6
#> 2 Oecanthus fultoni   Oregon 9.2007 -36.53   9  28 Fulton, 1925 4.622474 6
#> 3 Oecanthus fultoni     Ohio 8.2080 -38.61  18  31 Walker, 1962 5.434942 6
dolbear(t=25)
#>             species location      m      c min max       source  t        n
#> 1 Oecanthus fultoni     Iowa 7.7879 -30.21  15  25 Fulton, 1925 25 164.4875
#> 2 Oecanthus fultoni   Oregon 9.2007 -36.53   9  28 Fulton, 1925 25 193.4875
#> 3 Oecanthus fultoni     Ohio 8.2080 -38.61  18  31 Walker, 1962 25 166.5900
```
