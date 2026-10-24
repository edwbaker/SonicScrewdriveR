# An S4 class to represent a PseudoWave object that is converted to a Wave object when operated on.

An S4 class to represent a PseudoWave object that is converted to a Wave
object when operated on.

## Slots

- `type`:

  Type of PseudoWave (e.g. "noise")

- `subtype`:

  Subtype of PseudoWave (e.g. "white" if type is "noise")

- `scale`:

  The Wave channels are multiplied by this value

- `offset`:

  This value is added to the Wave channels

- `seed`:

  Random seed, so that the same PseudoWave gives the same samples every
  time. NA draws a fresh sequence on each use.

- `params`:

  List of additional parameters to pass to generating function
