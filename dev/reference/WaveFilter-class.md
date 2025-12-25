# WaveFilter object for audio filters

A `WaveFilter` object is an object containing information necessary for
the
[`filterWave()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/filterWave.md)
function to apply the filter to a `Wave` or `TaggedWave` object. This is
designed to allow a pipe operator (either magrittr or base R) to be used
to apply filters to a Wave in a pipeline. If used with a `TaggedWave`
object the function adds information to the `processing` slot
documenting its action.

## Slots

- `description`:

  Description of the filter.

- `func`:

  Name of function.

- `params`:

  List of additional parameters to pass to the function.
