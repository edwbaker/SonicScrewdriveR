# List the filters of an audioBlast module

audioBlast refuses a query that filters a module on a field it does not
have, so this function lists the fields that can be used as filters in
[`audioblast()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblast.md).

## Usage

``` r
audioblastFilters(name)
```

## Arguments

- name:

  Name of an audioBlast module, e.g. "recordings" or "aci".

## Value

A data frame with one row for each filter: its `name`, a `description`,
its `type`, whether it takes several values at once (`multiple`) and
whether it matches values that contain it (`contains`), rather than only
values equal to it.

## Examples

``` r
if (FALSE) { # \dontrun{
audioblastFilters("recordings")
} # }
```
