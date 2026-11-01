# Read AudioMoth metadata from a wave file

Deprecated. Reads and parses metadata stored in wave files produced by
AudioMoth devices.

## Usage

``` r
audiomothWave(filename)
```

## Arguments

- filename:

  Path to the wave file to read

## Value

A list of extracted parameters:

- raw:

  The comment string the other fields were read from

- start_time:

  Recording start time, as `"HH:MM:SS"`

- start_date:

  Recording start date, as `"DD/MM/YYYY"`

- time_zone:

  Time zone of the start time, e.g. `"UTC"` or `"UTC+1"`

- serial:

  The device serial number, or NA for a deployment ID file

- deployment_id:

  The configured deployment ID, or NA if none was set

- gain:

  Gain setting, one of "low", "low-medium", "medium", "medium-high" or
  "high"

- voltage:

  Battery voltage in volts. Where the firmware reported only that the
  battery was outside the range it measures, the threshold it compared
  against is returned with a leading `"<"` or `">"`, e.g. `"<2.5"`. The
  thresholds themselves have changed: firmware up to 1.2.0 wrote
  `"<3.6"` and `">5.0"`, 1.2.2 and 1.3.0 `"<3.6"` and `">4.9"`, and
  1.4.0 onwards `"<2.5"` and `">4.9"`

- temperature:

  Temperature in degrees Celsius, or NA for firmware before 1.4.0, which
  did not record it

- external_mic:

  Whether an external microphone was used

- filter:

  "Low-pass", "Band-pass", "High-pass", or FALSE if no filter was
  applied

- filter.limit:

  Filter frequency as the recorder writes it in its comment: text, in
  kHz, such as `"5.0"`, or two values separated by a hyphen for a
  band-pass filter, such as `"1.0-5.0"`. It is not converted to Hz, the
  unit of the frequencies the package measures, as it is a reading of
  the recorder's own record. FALSE if no filter was applied

- cancelled:

  Why the recording stopped early, or FALSE if it ran to completion

- unparsed:

  The fields the comment appeared to carry and that could not be read,
  where there are any. Absent otherwise

## Details

Deprecated in favour of
[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md),
which reads the AudioMoth comment along with every other kind of
metadata a file carries and returns these same fields as its
`sources$audiomoth`. It also normalises the recording time into a
`datetime`, which this function leaves as the separate `start_date`,
`start_time` and `time_zone` strings the recorder wrote.

The comment written into the wave header has been reworded several times
across AudioMoth-Firmware-Basic releases, most significantly in 1.6.0,
where "at medium gain setting while battery state was" became "at medium
gain while battery was" and the filter and cancellation clauses were
rephrased. Fields are matched by pattern rather than by position, so all
wordings from 1.0 onwards are read.

Firmware from 1.5.0 can be configured with a deployment ID, which the
firmware writes in place of the device serial number. Such files have a
`deployment_id` and no `serial`.

Firmware before 1.4.0 wrote a gain index rather than a gain name, so
`gain` is a number ("0" to "4") for files from those versions.

## Fields that were not read

A field this cannot read is NA, and is named in `unparsed`. Nothing is
reported as a default: `filter`, `filter.limit`, `cancelled` and
`external_mic` are FALSE only where the comment carries no clause of
that kind at all, which is how the firmware says that no filter was
applied, that the recording ran to completion and that the built-in
microphone was used — by writing nothing. Where it carries such a clause
in a wording this package does not know, the field is NA rather than
FALSE, since FALSE would be a claim about the recording that the file
never made.

`unparsed` is therefore what tells the two apart: a field that is NA and
is not named there has no value to be had, and one that is NA and is
named there is a wording this package needs to learn. A warning classed
`unparsedComment`, and `unparsedField`, carries the comment so that it
can be reported; class it to silence this one kind of warning while
reading a corpus that is known to hold wordings this package does not
know.

A comment cut short names nothing in `unparsed` and warns about nothing.
Every complete comment ends in a full stop, so one that does not was cut
— a version 1 ID3 tag allows a comment 30 bytes, which the recorder's
own wording fills before it reaches the end of the date — and what is
past the cut is absent from the file rather than unread. There is no
wording to report in that case, only a shorter comment than the recorder
wrote.

## See also

[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md),
which reads this alongside every other kind of metadata a file holds,
and whose `sources$audiomoth` is what this function returns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Deprecated. Prefer:
readMetadata("./FILENAME.WAV")$sources$audiomoth
} # }
```
