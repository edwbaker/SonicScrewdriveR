# Flatten the output of a process into rows

The output of a process record can be anything a function returns: a
single number, a named vector of indices, a data frame of detected
events, a list of all of these, or nothing. `flattenOutput()` turns any
of them into a data frame, so that the results of a process applied to
many recordings, or to many windows of one, can be bound into one table.

## Usage

``` r
flattenOutput(x)
```

## Arguments

- x:

  The output of a process, as held in the `output` of a record in the
  `processing` slot of a `TaggedWave`, or any other result.

## Value

A data frame, with one row unless `x` is `NULL` or a data frame.

## Details

Every column name begins with `value`. A table of results needs columns
of its own beside these, to say which recording, which window and which
process a row came from, and those can be given any name that does not
begin with `value` without risk of clashing with a result.

The rule is:

- `NULL`:

  No rows. There was nothing to flatten.

- A data frame:

  Its rows, as they are: a detector that found five events gives five
  rows, and one that found none gives none. Each column is kept as it
  is, its name following `value.`, so a column `start` is `value.start`.
  Row names the data frame was given, rather than the numbers it has by
  default, are kept in a column of their own, `value.rownames`.

- Anything else:

  One row, as below.

Within that one row:

- A single value:

  One column, `value`.

- A named vector:

  One column for each element, its name following `value.`:
  `c(mean=440, sd=7.5)` gives `value.mean` and `value.sd`.

- A vector without names:

  One column, `value`, holding the whole vector in a list cell.

- A named list:

  Each element by these same rules, the names of an element and of what
  it holds joined by a full stop: `list(n=2, b=c(x=2, y=3))` gives
  `value.n`, `value.b.x` and `value.b.y`. An element that is `NULL`
  gives no column.

- A data frame within a list:

  One column for each of its columns, holding the value where it has one
  row and the whole column in a list cell otherwise.

- Anything else:

  One column, `value`, holding it in a list cell. This takes in a
  matrix, an S4 object, a list with a class of its own such as a test
  result, and a list or vector some of whose names are missing or
  repeated, whose elements could not each be given a column of their
  own.

A name the rule would give twice, as `list(a.b=1, a=list(b=2))` would,
is made unique with
[`make.unique()`](https://rdrr.io/r/base/make.unique.html).

Which columns there are depends on the names in the output and never on
how long its parts are, so that the rows from different recordings line
up. What a column holds can depend on length, though: an unnamed vector
of one value and one of five both give `value`, the first as a plain
value and the second in a list cell, and a data frame within a list
gives plain values for one row and list cells for more. A detector that
finds one peak in one window and three in the next therefore gives the
same column of two types, and a table binding such rows needs to hold
that column as a list wherever any row has a list cell in it. A column
can also be missing from some rows, as where an element is `NULL` in one
output and not in another.

The `processing` attribute
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md)
puts on a result that is not a wave is dropped, as it is the history of
the wave rather than part of the result.

## See also

[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md),
which makes the records.

## Examples

``` r
flattenOutput(0.42)
#>   value
#> 1  0.42
flattenOutput(c(mean = 440, sd = 7.5))
#>   value.mean value.sd
#> 1        440      7.5
flattenOutput(list(n = 2, peaks = c(440, 880)))
#>   value.n value.peaks
#> 1       2    440, 880
flattenOutput(data.frame(start = c(0.1, 0.6), end = c(0.3, 0.9)))
#>   value.start value.end
#> 1         0.1       0.3
#> 2         0.6       0.9
```
