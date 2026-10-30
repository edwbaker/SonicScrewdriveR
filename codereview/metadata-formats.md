# Correctness review: metadata formats and chunk parsing

Scope: `R/guano.R`, `R/bext.R`, `R/id3.R`, `R/xmp.R`, `R/vorbisComment.R`, `R/wamd.R`,
`R/wavChunkMap.R`, `R/wavChunks.R`, `R/wavCues.R`, `R/wavMetadata.R`, `R/audiomoth.R`,
`R/audiomothTWav.R`, `R/audacity.R`, `R/Annotation.R`, `R/labels.R`, `R/metadataUtils.R`,
`R/readMetadata.R`, `R/readAudio.R`, `R/writeAudio.R`, `R/parseFilename.R`, `R/TaggedWave.R`,
`R/tagApply.R`, `R/data2Wave.R`, `R/respeaker6.R`, `R/olym.R`.

Correctness only. Seven findings, most severe first. Evidence is hand-built byte-level
fixtures fed to the package's own parsers, plus the package's own `inst/extdata` files.
See **Caveats** for what could not be exercised.

---

## 1. `R/wavChunks.R:721` — a legitimate non-zero pad byte misaligns the walk and loses every chunk after it — CONFIRMED

RIFF requires the body of an odd-sized chunk to be followed by a pad byte, but does not
specify that byte's value; a reader must ignore it. `.wavUnpadded()` instead treats any
non-zero byte at the pad position as proof that the writer omitted the pad, and steps the
walk back one byte. The only guard is `.wavIsChunkHeader()`, which asks merely that four
bytes be printable ASCII and that the size read after them fit in the remaining file —
and the bytes it examines at `pos - 1` are `(pad, id1, id2, id3)` with the size taken from
`(id4, sz1, sz2, sz3)`, so the "size" is garbage. For a small chunk sitting ahead of a long
recording that garbage size routinely fits, and the guard passes.

Failure scenario (reproduced): two files identical but for one byte — `fmt ` (16), an
odd-sized `bext` of 603 bytes, a 256-byte `LIST`/`INFO` carrying `ICMT`, a 44-byte `guan`,
then 70000 bytes of `data`; file size 70972 either way.

With the pad byte `0x00`:

```
RIFF WAVE, 5 chunks
    id start  size truncated form
1 fmt     21    16     FALSE <NA>
2 bext    45   603     FALSE <NA>
3 LIST   657   256     FALSE INFO
4 guan   921    44     FALSE <NA>
5 data   973 70000     FALSE <NA>
  guano   :  GUANO|Version, Make
  wavInfo :  ICMT
  .wavFormat:  dataStart=973 dataSize=70000
```

With the pad byte `0x58` (`'X'`) — equally valid RIFF:

```
RIFF WAVE, 4 chunks
    id start  size truncated form
1 fmt     21    16     FALSE <NA>
2 bext    45   603     FALSE <NA>
3 XLIS   656 65620     FALSE <NA>
4 ???? 66284  4689      TRUE <NA>
  guano   :
  wavInfo :
  .wavFormat:  ERROR: Could not find DATA chunk ID.
```

`wavChunks()` invents two chunks; `guano()` and `wavInfo()` report the file as carrying
nothing; `.wavFormat()` raises `unsupportedWav`, so `readAudio()` abandons the `readWave`
path for a file that is not malformed. `bext` survives only because it precedes the
misalignment. The same byte sequence read through `.mdWavMap()` is affected identically,
since both go through `.wavWalk()`.

The omitted-pad repair the function is for does work (a file written with the pad left out
is recovered correctly), so the fix is to tighten the step-back rather than drop it: require
the recovered identifier to be one the file plausibly holds, or require the *padded*
position not to look like a chunk header before preferring the unpadded one.

## 2. `R/id3.R:45` — UTF-16 byte order is taken from the frame's description and applied to its value, corrupting conforming values — CONFIRMED

In ID3v2 every string in an encoding-`$01` frame carries its own byte order mark.
`.id3Value()` calls `.id3Encoding(as.integer(data[1]), rest)` once, at `R/id3.R:168` for
`COMM`/`USLT` and `R/id3.R:185` for text frames (and `R/id3.R:201` for `WXXX`), where `rest`
begins at the *description*. The result is then used to decode the *value*. An empty
description is written as the bare two-byte terminator `00 00`, which is not a BOM, so
`.id3Encoding()` falls through to `"UTF-16BE"` at line 45 and applies big-endian decoding to
a value that begins `FF FE`.

Failure scenario (reproduced): a v2.4 `COMM` frame, encoding byte `01`, language `eng`,
empty description (`00 00`), value `FF FE 64 00 75 00 73 00 6B 00` — UTF-16LE `"dusk"` with
its BOM, which is fully conforming:

```
$COMM
[1] "<ef><bf><be><U+6400><U+7500><U+7300><U+6B00>"     # want "dusk"
```

The BOM is decoded as U+FFFE and every character byte-swapped. `TXXX` and `WXXX` take the
same path through `.id3Described()`.

Related, same line, also reproduced: a text frame declaring encoding `01` whose text is
UTF-16LE with no BOM at all — non-conforming, but what several Windows taggers emit — reads
as the same CJK garbage (`TIT2` → `"<U+6400><U+7500><U+7300><U+6B00>"`). The chosen
fallback, `UTF-16BE`, is the less likely of the two orders in the wild.

Everything else in the ID3 reader checked out: all four encoding bytes on conforming input,
v2.2 three-character frames with 3-byte sizes, v2.3 plain big-endian frame sizes against
v2.4 sync-safe, the extended-header size convention differing between v2.3 and v2.4, the
v2.4 data-length-indicator and grouping flags, whole-tag v2.3 unsynchronisation, nul-padding
after the last frame, a frame declaring a size past the end of the tag, and v1.1 field
offsets including the track/genre discrimination.

## 3. `R/readMetadata.R:827` — the AudioMoth comment is parsed for WAVE and MP3 but not for FLAC or Ogg, so a FLAC loses its recording time, make, model and serial — CONFIRMED

`R/readMetadata.R:819-826` runs `.mdAudiomothSource()` for wave and MP3 containers, with the
comment at line 821 explaining why: a transcoded recording keeps the recorder's comment in
its tag. The FLAC/Ogg branch at lines 827-830 reads the Vorbis comments and an ID3 tag but
never calls `.mdAudiomothSource()`, and `.mdNormaliseVorbis()` derives no `datetime` except
from a `DATE` field. The comment is sitting right there in `COMMENT`/`DESCRIPTION`.

Failure scenario (reproduced on the package's own fixtures, which are the same recording in
two containers):

```
readMetadata("inst/extdata/AUDIOMOTH.mp3")$fields
  datetime     2021-05-01 23:32:00
  make         Open Acoustic Devices
  model        AudioMoth
  serial       240435055C7B17DB

readMetadata("inst/extdata/AUDIOMOTH.flac")$fields
  software     reference libFLAC 1.4.2 20221022
  recordist    AudioMoth 240435055C7B17DB
  comment      Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at ...
```

No `datetime`, `make`, `model` or `serial` for the FLAC, although `vorbisComment()` on that
file returns both `COMMENT` and `DESCRIPTION` holding the identical
`"Recorded at 23:32:00 01/05/2021 (UTC) by AudioMoth 240435055C7B17DB at medium gain setting
while battery state was 4.1V and temperature was 5.8C."`. Passing those fields to
`.mdAudiomothSource()` would fill all four.

## 4. `R/audiomoth.R:232` — an unrecognised comment wording reports `filter = FALSE` and `cancelled = FALSE`, which assert facts rather than ignorance — CONFIRMED

`filter` and `filter_limit` (lines 232-233) and `cancelled` (line 258) are initialised to
`FALSE` and replaced only on a regex match. `FALSE` is documented to mean "no filter was
applied" and "it ran to completion" — positive claims about the recording. A wording the
parser does not match therefore yields a confident wrong answer, not a missing one. On the
same path `gain`, `voltage` and `temperature` come back `NA` with no warning, so nothing
distinguishes "this firmware did not record it" (true of temperature before 1.4.0, which the
documentation relies on) from "this wording was not understood".

Failure scenario (reproduced): a comment reworded as
`"Recorded at 19:22:00 06/04/2019 (UTC) by AudioMoth 24F3190361DC3B0C with gain set to
medium, battery at 4.5V, temperature 11.2 degrees C."` gives

```
gain=NA  volt=NA  temp=NA  filter=FALSE  filter.limit=FALSE  cancelled=FALSE
```

with the date, time, zone and serial read correctly — so the result looks like a successful
read of a recording made with no filter that ran to completion.

Every wording the source documents does parse correctly, which is worth recording: the 1.0
gain-index and two-decimal voltage form, the `< 3.6V` and `less than 3.6V` battery forms, the
1.4.0 named gain with temperature, the 1.5.0 deployment ID, the 1.6.0 rewording
("at medium gain while battery was"), both filter phrasings, band-pass, both cancellation
phrasings plus the magnetic-switch rewording, the external microphone, a `UTC+1` zone, a
negative temperature, and 1.8-style amplitude-threshold and frequency-trigger clauses
(including that the trigger's own kHz value is not mistaken for the filter's). The exposure
is a future firmware release, not the existing corpus.

## 5. `R/parseFilename.R:242` — a filename whose digits do not spell a real date gives `datetime = NA` with no warning, indistinguishable from a format that carries no time — CONFIRMED

`.parseWallClock()` returns whatever `strptime()` gives and nothing checks it. `NA` is also
what the documented contract uses for a format carrying no time ("A format carrying no time,
which TSA names do not, reports `datetime` as NA"), so a caller cannot tell the two apart.

Failure scenario (reproduced):

```
20240230_120000.wav   match=YYYYMMDD_HHMMSS  datetime=NA     # 30 February
20241301_120000.wav   match=YYYYMMDD_HHMMSS  datetime=NA     # month 13
20240101_250000.wav   match=YYYYMMDD_HHMMSS  datetime=NA     # hour 25
20240101_006100.wav   match=YYYYMMDD_HHMMSS  datetime=NA     # minute 61
20240230.wav          match=YYYYMMDD         datetime=NA
recording.wav         match=YYYYMMDD         datetime=NA     # format forced by the caller
```

`readMetadata()` is not affected — `.mdNormaliseFilename()` drops a non-finite datetime, so
no `datetime` field is reported — but `parseFilename()` is exported and documented to return
one, and a direct caller gets a silent `NA`.

Timezone handling, which was the other thing to check here, is correct: the instant tracks
the declared zone exactly (`20200609_191730.wav` → epoch 1591730250 at `timezone="UTC"`,
1591723050 at `"Europe/Berlin"`, 1591744650 at `"America/New_York"`) and does not move with
the session's own `TZ`.

## 6. `R/respeaker6.R:31` — reading a single sample frame drops the matrix dimensions and the next line fails — PLAUSIBLE

`w@.Data[,1:6]` uses R's default `drop=TRUE`. For a one-row matrix that yields a plain
vector of length 6, and `colnames(w@.Data) <- ...` at line 32 then raises "attempt to set
'colnames' on an object with less than two dimensions". `readRespeaker6(f, from=1, to=1)` on
an eight-channel file should hit it; `drop=FALSE` is the fix.

Not reproduced: `tuneR` could not be installed in this container (see **Caveats**), so
`readWave()` could not be run. The reasoning rests only on base R's `[` semantics
(`dim(matrix(1:8, nrow=1)[,1:6])` is `NULL`), not on tuneR behaviour.

## 7. `R/audacity.R:32` — a label file with fewer than three columns fails with a message about the `names` attribute rather than about the file — CONFIRMED

`colnames(labels)[1:3] <- c("from", "to", "label")` assigns three names to a data frame that
may have two columns.

Failure scenario (reproduced): a file holding the single line `0.000000<TAB>1.000000`, with
no label field, gives

```
Error: 'names' attribute [3] must be the same length as the vector [2]
```

Audacity itself always writes the third field — an empty label gets a trailing tab, which
reads correctly — so this is reachable only from a hand-edited or third-party file. Lowest
severity, listed because malformed input should say what is wrong with it.

---

## Checked and found correct

Recording these so they need not be re-audited. Each was exercised, not just read.

**The odd-byte pad rule itself** (beyond finding 1). A correctly padded odd-sized chunk is
walked correctly; a chunk whose writer omitted the pad is recovered and the chunks behind it
are found (`odd `(5) at 45, then `data` at 58 rather than 59); a zero-size chunk advances the
walk by the 8 header bytes and does not loop; a chunk declaring a size past the end of the
file is reported with the size it has and `truncated = TRUE`; a file ending mid-header stops
cleanly. `.infoComment()`, `.infoFields()`, `.wavCueLabels()` and `.wamdFields()` all apply
the same pad rule to the fields inside a chunk and all advance unconditionally on a
zero-size field.

**Offset arithmetic.** Zero-based offsets are converted to one-based R indices correctly
throughout: cue points (`dwName` at body `[5:8]`, `fccChunk` at `[13:16]`, `dwSampleOffset`
at `[25:28]`), `ltxt` (4-byte name, 4-byte sample length, 4-byte purpose, then four 2-byte
language fields = 20 bytes before the text), `ds64` (three 8-byte sizes, table count at
offset 24, 12-byte entries from 28), `olym` (`.olymAt()`'s `body[where[1] + seq_len(...)]`),
and the bext field table. Sizes are read after the 8-byte header everywhere. No parser can
read past end of file: every loop is bounded by `pos + 7L <= n` and every body read by
`min(chunk$size, cap)`.

**`bext` `TimeReference`.** Both 32-bit halves are read and combined correctly.
`.rawToUInt(body[339:346])` treats the two fields as one little-endian 64-bit value, which is
exactly right given low-then-high ordering: low 48000 / high 0 → 48000; low 0 / high 1 →
4294967296; low 123 / high 2 → 8589934715. `CodingHistory` is read from byte 603 to the end
of the chunk, `.BEXT_FIXED` of 602 is right for a version-2 chunk, and a chunk truncated at
300 bytes yields the fields that fit rather than an error.

**GUANO against the specification.** `GUANO|Version` is written first and added if absent;
namespaced keys (`OAD|Battery Voltage`) round-trip; the key ends at the first colon, so a
`Timestamp` value containing colons parses; `Note` escapes and un-escapes `\n` as the two
characters, and the specification confirms that escape "applies exclusively to fields which
specify multiline string as their data type", of which `Note` is the only defined one — so
leaving other values alone is correct, not an omission. Every typed field survives a
`guanoWrite()` → `guano()` round trip through a real file: `Samplerate` (integer),
`Loc Position` (two floats, 15 significant digits), `Length` (float), `TE`, `Tags` (list),
`Note` (multiline) and `Timestamp` with a `+02:00` offset preserved as an offset.

**`guanoWrite()`.** Appending to a file with no `guan` chunk, appending after an odd number of
audio bytes (a pad byte is inserted and the sizes stay consistent), growing and shrinking an
existing trailing chunk, and growing a chunk that *precedes* the audio — where the data chunk
moved by exactly 512 bytes, keeping the AudioMoth block alignment the code is careful about.
In every case `file.size() == riffSize + 8`.

**`wavCues()`.** Positions are sample offsets and are not confused with seconds: a cue at
sample 48000 in a 48 kHz file reports `position = 48000`, `seconds = 1`, and an `ltxt` region
of `dwSampleLength = 24000` becomes an Annotation running from 2 s to 2.5 s. Also correct: a
cue chunk declaring more points than it holds is read as far as it goes, a label whose cue
point is missing is reported with no position, and a file with no cues gives an empty result.

**AudioMoth T.WAV.** The 512-byte block encoding, the `2^(0:31)` bit decode, and the
expansion plan match the reference implementation. Verified against
`inst/extdata/AUDIOMOTHT.WAV`: one encoded block at file block 65 with count 64, block 1
correctly excluded for lying outside the data chunk, growth 64×512 − 512 = 32256 bytes, and
the expanded audio 99816 bytes = 32280 + 32768 + 34768, which reconstructs the segment
boundaries exactly. `OpenAcousticDevices/AudioMoth-Utils/expander.js` computes
`inputBytes = ENCODED_BLOCK_SIZE_IN_BYTES; outputBytes = numberOfBlocks *
ENCODED_BLOCK_SIZE_IN_BYTES` and treats a count of zero as audio — identical to this
package on both points. `.AM_HEADER_SIZE` of 488 matches the header `.audiomothHeader()`
actually builds (12 + 24 + 444 + 8), and `audiomothCompress()`'s segment arithmetic
(`firstSegment = (segment - 488) %/% 2`) keeps every later segment on a 512-byte boundary.

**Date and time parsing.** `.mdDate()` reads the ISO forms and rejects what it should:
`+02:00` offsets, `Z`, bare wall clocks, dates alone, one-digit fields, fractional seconds,
and an offset after a space; it returns NULL for hour 25, 30 February, `14/06/2024` and a
bare year. `.mdAudiomothDatetime()` parses `DD/MM/YYYY` (confirmed with day 13, and
`04/13/2019` correctly refused) and subtracts the recorder's configured offset in the right
direction for `UTC+1`, `UTC-5`, `UTC+1:30`, `UTC-3:45` and `UTC+14`. `.wamdTimestamp()`
handles `-04:00`, `-0400`, no offset, `Z`, one-digit fields and minute-only times, and keeps
an unreadable value as text with a warning. `.olymTime()`'s two-digit year pivot at 70 is as
documented.

**Other readers.** `wamd` field walk, unknown field numbers kept as `wamd:21`, binary fields
reported by size only, hemisphere letters and signed coordinates both read, altitude
optional. `xmp` reads properties written as attributes, as elements and as
`rdf:Alt`/`Bag`/`Seq`, prefers the `x-default` entry, and unescapes `&amp;` last so
`&amp;lt;` is not turned into `<`. `vorbisComment` on `inst/extdata/AUDIOMOTH.flac` walks the
FLAC metadata blocks to type 4 and reads vendor plus all fields. `readAudacityLabels()`
handles the plain three-column form, Audacity's backslash frequency-continuation rows
(including when only some labels carry them), empty labels, the literal text `NA`, embedded
double quotes, decimal commas, an empty file, and a stray leading continuation row.
`labelReductionExecute()` sorts before merging, so a region wholly inside another is merged
once. Every reader returns an empty object rather than an error for `inst/extdata/EMPTY.WAV`
(0 bytes), and `wavChunks()`/`wavCues()` raise a clear "File is too short to be a WAV file."

**`mime` routing in `writeAudio()`.** `guess_type()` returns `audio/x-wav` for `.wav`,
`.WAV` and `.Wav`, so the uppercase names AudioMoth writes take the `tuneR::writeWave()`
path as intended.

---

## Caveats

**`tuneR` could not be installed, so two things in the brief were not exercised.** CRAN and
Posit Package Manager are blocked by this container's network policy (the proxy answers 403
to `CONNECT cloud.r-project.org:443`); Ubuntu noble ships no `r-cran-tuner`, `r-cran-seewave`
or `r-cran-signal`; and `R CMD INSTALL` of the CRAN sources cloned from GitHub was refused by
the sandbox as external code. R 4.3.3 itself, `testthat`, `stringi`, `digest`, `ggplot2`,
`hms`, `httr2`, `jsonlite`, `mime` and `Rdpack` are installed.

Consequently:

- **The `readAudio()` → `writeAudio()` → `readAudio()` round trip was not run**, and this
  report makes no claim about it. Whether `writeAudio()` can produce a file the package's own
  reader rejects is open. What *was* checked statically: `writeAudio()` routes `.wav`/`.WAV`
  to `tuneR::writeWave()` and everything else through `av`, and `.wavFmtFields()` reads the
  `WAVE_FORMAT_EXTENSIBLE` sub-format that `tuneR` writes, so the reader understands the
  writer's format in principle.
- **`R/TaggedWave.R`, `R/tagApply.R` and `R/data2Wave.R` were read but not run**, since all
  three need `tuneR`'s S4 `Wave` class. Nothing in them was confirmed and nothing is reported
  against them. On reading, the provenance machinery looks sound: `.tagApplyOne()` passes
  `...` to `.fun` unaltered, `.recState$active` with `on.exit()` keeps only the outermost call
  recorded, the record's `func`/`params`/`time`/`duration`/`input_hash` are built from the
  actual call, `output_hash` is taken after `.fun` returns, and `origin`/`metadata`/
  `processing` are carried onto an untagged result while `spectrogram_maad_power` is
  deliberately not. The one thing I would test first with `tuneR` available is whether
  `.needsUntag()` correctly leaves this package's own functions tagged while untagging
  `seewave`'s, since it keys on `topenv()` identity.
- The `AudioMoth HEX` and Wildlife Acoustics branches of `parseFilename()` call
  `seewave::audiomoth()` and `seewave::songmeter()` and were likewise not run.

A `signal` shim was installed to try to satisfy `tuneR`'s imports; since `tuneR` never
installed, no finding in this report passed through it. Nothing else was stubbed — the pure-R
parsing files were sourced directly and every CONFIRMED finding above was reproduced against
real bytes with no dependency in the path.
