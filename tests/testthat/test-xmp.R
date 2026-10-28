# The XMP packet a wave file carries in its _PMX chunk. RDF lets one property
# be written as an attribute, as an element, or as a list, so each shape is
# tested on its own as well as together.

#' An XMP packet holding each shape a property can be written in
mdXmpText <- function(padding = 0L) {
  paste0(
    paste(
      "<?xpacket begin=\"﻿\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>",
      "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\" x:xmptk=\"3.1.2-113\">",
      " <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">",
      "  <rdf:Description rdf:about=\"\"",
      "    xmlns:xmpDM=\"http://ns.adobe.com/xmp/1.0/DynamicMedia/\"",
      "    xmlns:dc=\"http://purl.org/dc/elements/1.1/\"",
      "    xmlns:xmp=\"http://ns.adobe.com/xap/1.0/\"",
      "   xmpDM:audioSampleRate=\"44100\"",
      "   xmpDM:tempo=\"120.6\"",
      "   xmpDM:stretchMode=\"Durée fixe\"",
      "   xmp:CreateDate=\"2019-03-15T08:14:00\">",
      "   <dc:format>audio/wav</dc:format>",
      "   <xmpDM:logComment>wind &lt;5 knots, A&amp;B</xmpDM:logComment>",
      "   <dc:title>",
      "    <rdf:Alt>",
      "     <rdf:li xml:lang=\"fr\">Le chant</rdf:li>",
      "     <rdf:li xml:lang=\"x-default\">Dawn chorus</rdf:li>",
      "    </rdf:Alt>",
      "   </dc:title>",
      "   <dc:creator>",
      "    <rdf:Seq>",
      "     <rdf:li>K Riede</rdf:li>",
      "     <rdf:li>E Baker</rdf:li>",
      "    </rdf:Seq>",
      "   </dc:creator>",
      "  </rdf:Description>",
      " </rdf:RDF>",
      "</x:xmpmeta>",
      sep = "\n"
    ),
    strrep(" ", padding),
    "\n<?xpacket end=\"w\"?>"
  )
}

#' A wave file carrying the given XMP packet
mdXmpWav <- function(text = mdXmpText()) {
  mdWav(mdFmt(), mdChunk("_PMX", mdUtf8(text)), mdData())
}

test_that("A property written as an attribute is read under its own name", {
  f <- mdFile(mdXmpWav())
  x <- xmp(f)

  expect_s3_class(x, "xmp")
  expect_identical(x[["xmpDM:audioSampleRate"]], "44100")
  expect_identical(x[["xmpDM:tempo"]], "120.6")
  expect_identical(x[["x:xmptk"]], "3.1.2-113")
})

test_that("A property written as an element is read under its own name", {
  x <- xmp(mdFile(mdXmpWav()))

  expect_identical(x[["dc:format"]], "audio/wav")
})

test_that("The escaping that XML puts on a value is undone", {
  x <- xmp(mdFile(mdXmpWav()))

  expect_identical(x[["xmpDM:logComment"]], "wind <5 knots, A&B")
})

# The packet declares itself UTF-8, and the editors that write these packets
# are localised, so a value can carry anything.
test_that("A value is read as the UTF-8 the packet declares", {
  x <- xmp(mdFile(mdXmpWav()))

  expect_identical(x[["xmpDM:stretchMode"]], "Durée fixe")
})

test_that("A list of alternatives gives the one marked as the default", {
  x <- xmp(mdFile(mdXmpWav()))

  expect_identical(x[["dc:title"]], "Dawn chorus")
  expect_false("dc:title.2" %in% names(x))
})

test_that("An ordered list with nothing marked gives every entry", {
  x <- xmp(mdFile(mdXmpWav()))

  expect_identical(x[["dc:creator"]], "K Riede")
  expect_identical(x[["dc:creator.2"]], "E Baker")
})

# An entry of a list belongs to the property that holds it. Reporting it under
# its own name would say the file has a property called rdf:li.
test_that("The entries of a list are not reported under the name rdf:li", {
  x <- xmp(mdFile(mdXmpWav()))

  expect_false(any(grepl("^rdf:li", names(x))))
})

test_that("A namespace declaration is not read as a property", {
  x <- xmp(mdFile(mdXmpWav()))

  expect_false(any(grepl("^xmlns:", names(x))))
})

# rdf:about says what the description is about, and xml:lang which language an
# entry is in. Neither is something the recording carries.
test_that("The structural attributes of the document are not read", {
  x <- xmp(mdFile(mdXmpWav()))

  expect_false("rdf:about" %in% names(x))
  expect_false("xml:lang" %in% names(x))
})

# A writer pads the packet so that a later one can rewrite it in place without
# moving the audio. The padding is part of the packet, and so part of what is
# kept on the result, but it says nothing and gives no property.
test_that("The padding a writer leaves in the packet gives no property", {
  properties <- function(x) {
    x <- unclass(x)
    attr(x, "xml") <- NULL
    return(x)
  }
  plain <- xmp(mdFile(mdXmpWav()))
  padded <- xmp(mdFile(mdXmpWav(mdXmpText(padding = 2000L))))

  expect_identical(properties(padded), properties(plain))
})

test_that("The packet itself is kept on the result", {
  expect_match(attr(xmp(mdFile(mdXmpWav())), "xml"), "^<\\?xpacket begin=")
})

test_that("A file with no _PMX chunk gives an empty result", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_s3_class(xmp(f), "xmp")
  expect_length(xmp(f), 0)
})

test_that("A _PMX chunk holding nothing but padding gives an empty result", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("_PMX", mdUtf8(strrep(" ", 64L))), mdData()))

  expect_length(xmp(f), 0)
})

test_that("A _PMX chunk holding something that is not a packet gives nothing", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("_PMX", mdUtf8("not a packet at all")), mdData()))

  expect_s3_class(xmp(f), "xmp")
  expect_length(xmp(f), 0)
})

test_that("The bytes of a file are read as the file is", {
  bytes <- mdXmpWav()

  expect_identical(unclass(xmp(bytes)), unclass(xmp(mdFile(bytes))))
})

test_that("An xmp object given back is the one that was given", {
  x <- xmp(mdFile(mdXmpWav()))

  expect_identical(xmp(x), x)
})

test_that("A file that is not audio gives an empty result", {
  f <- mdFile(charToRaw("this is not audio, not even slightly"), ".wav")

  expect_length(xmp(f), 0)
})

test_that("A file that does not exist says so", {
  expect_error(xmp(file.path(tempdir(), "no-such-recording.wav")), "does not exist")
})

test_that("An empty result says which format it found nothing of", {
  f <- mdFile(mdWav(mdFmt(), mdData()))

  expect_output(print(xmp(f)), "<no XMP metadata>")
})

# ------------------------------------------------- the normalised record ---

test_that("What the editor was told is reported in the normalised record", {
  m <- readMetadata(mdFile(mdXmpWav()), filename = FALSE)

  expect_identical(m$fields$title, "Dawn chorus")
  expect_identical(m$fields$recordist, "K Riede")
  expect_identical(m$fields$comment, "wind <5 knots, A&B")
  # A packet that gives no offset is read in the local zone, as every other
  # format this package reads is, so the reading is checked as it is written
  # rather than against a zone the test machine may not be in.
  expect_identical(
    format(m$fields$datetime, "%Y-%m-%dT%H:%M:%S"),
    "2019-03-15T08:14:00"
  )
})

# The sample rate in a packet is what an editor read, which a file damaged
# since can contradict. The file's own header is the better answer, so the
# packet's reading is left where a caller can see it is the packet's.
test_that("The audio properties of a packet stay in the packet", {
  m <- readMetadata(mdFile(mdXmpWav()), filename = FALSE)

  expect_identical(m$sources$xmp[["xmpDM:audioSampleRate"]], "44100")
  expect_false("samp_rate" %in% names(m$fields))
})

test_that("A packet is no longer counted among the chunks nothing reads", {
  m <- readMetadata(mdFile(mdXmpWav()), filename = FALSE)

  expect_false("_PMX" %in% m$unparsed)
  expect_true("xmp" %in% names(m$sources))
})
