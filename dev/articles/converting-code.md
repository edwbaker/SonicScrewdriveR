# Converting code to work with SonicScrewdriveR

``` r
library(sonicscrewdriver)
```

## Introduction

SonicScrewdriveR introduces several tools for working with audio data,
such as the `TaggedWave` and `TaggedWaveMC` formats. Inside
SonicScrewdriveR the Tagged and non-Tagged versions of `Wave`-like
objects can be used interchangeably, however this is not necessarily
true for functions from other packages.

This guide is aimed both at end users and package developers who want to
convert their code to work with SonicScrewdriveR.

### TaggedWave and TaggedWaveMC

``` r
# Generate a `Wave` and `TaggedWave` sine
w <- tuneR::sine(440)
tw <- tagWave(w)
```

If we try to use a function that expects a `Wave` object with a
`TaggedWave` object, we will get an error.

``` r
# This will fail
seewave::oscillo(tw)
#> Error in `seewave::oscillo()`:
#> ! argument "f" is missing, with no default
```

As an end user you can use the `untagWave` function to convert a
`TaggedWave` object to a `Wave` object.

``` r
# This will work
seewave::oscillo(untagWave(tw))
```

The `untagWave` function can also be used to ‘untag’ a `TaggedWaveMC`
object.

To fix this in your own function or package you can make use of
[`inherits()`](https://rdrr.io/r/base/class.html) rather than
alternative methods.

``` r
# Methods that fail
is(w, "Wave")
class(w) == "Wave"

# Method that works
inherits(w, "Wave")
```

As `TaggedWave` inherits from `Wave` it can be treated like a `Wave` in
your code.

The same technique can be used for `TaggedWaveMC` objects with
`inherits(w, "WaveMC")`.

#### Adding a process to a Tagged Wave-like object

If you want to add a process to a `TaggedWave` object you can use the
`addProcess` method. This can either be called by the end user, or by
functions that are aware of the Tagged versions.

``` r
# End user using non-Tagged aware function
seewave::oscillo(untag(tw))
addProcess(tw, "seewave::oscillo")

# Inside a Tagged aware function
addProcess(
  tw,
  process = "mypkg::functionToCountChannels",
  output = list(channels = 2)
)
```
