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

  Random seed for reproducible output, NA for no seed

- `scale`:

  Logical. Whether to use the random seed value

- `params`:

  List of additional parameters to pass to generating function
