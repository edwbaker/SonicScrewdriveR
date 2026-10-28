test_that("wavChunks() lists the chunks a file holds, in the order it holds them", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("bext", raw(10)), mdData(16)))
  got <- wavChunks(f)
  expect_s3_class(got, "wavChunks")
  expect_s3_class(got, "data.frame")
  expect_identical(got$id, c("fmt ", "bext", "data"))
  expect_identical(got$size, c(16, 10, 16))
  # The RIFF header is twelve bytes, and each chunk header another eight.
  expect_identical(got$start, c(21, 45, 63))
  expect_false(any(got$truncated))
})

test_that("The map a file gives and the map its bytes give are the same file", {
  f <- mdFile(mdWav(mdFmt(), mdGuanoChunk("Make:Open Acoustic Devices"), mdData()))
  bytes <- readBin(f, "raw", n=file.size(f))
  fromFile <- wavChunks(f)
  fromBytes <- wavChunks(bytes)
  expect_identical(attr(fromFile, "file"), f)
  expect_identical(attr(fromBytes, "file"), NA_character_)
  # Everything but where it was read from.
  expect_identical(unclass(fromFile)[names(fromFile)],
                   unclass(fromBytes)[names(fromBytes)])
})

test_that("The kind of a list is reported, and nothing else claims one", {
  f <- mdFile(mdWav(
    mdFmt(),
    mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("INAM", "A Title"))),
    mdChunk("LIST", c(charToRaw("adtl"), mdCueField("labl", 1L, "A Label"))),
    mdData()
  ))
  got <- wavChunks(f)
  expect_identical(got$id, c("fmt ", "LIST", "LIST", "data"))
  expect_identical(got$form, c(NA, "INFO", "adtl", NA))
})

test_that("The form and the size the header declares are reported", {
  f <- mdFile(mdWav(mdFmt(), mdData(32)))
  got <- wavChunks(f)
  expect_identical(attr(got, "wavForm"), "RIFF")
  expect_identical(attr(got, "riffSize"), file.size(f) - 8)
})

test_that("A chunk cut short is flagged rather than refused", {
  bytes <- mdWav(mdFmt(), mdData(400))
  cut <- bytes[seq_len(length(bytes) - 200L)]
  got <- wavChunks(cut)
  expect_identical(got$id, c("fmt ", "data"))
  expect_identical(got$truncated, c(FALSE, TRUE))
  expect_identical(got$size[2], 200)
})

test_that("A file that is not a wave file is refused, and says why", {
  expect_error(wavChunks(mdFile(charToRaw("ID3\u0003not a wave file at all"))),
               class="unsupportedWav")
  expect_error(wavChunks(mdFile(raw(4))), class="unsupportedWav")
})

test_that("A path that names no file, or a directory, is an error of its own", {
  expect_error(wavChunks(file.path(tempdir(), "no-such-file.wav")),
               "File does not exist")
  expect_error(wavChunks(tempdir()), "not a file")
})

test_that("Printing the chunks says what the file is before listing them", {
  f <- mdFile(mdWav(mdFmt(), mdData()))
  expect_output(print(wavChunks(f)), "RIFF WAVE, 2 chunks")
  one <- wavChunks(mdFile(mdWav(mdFmt())))
  expect_output(print(one), "RIFF WAVE, 1 chunk\\b")
})

# ----------------------------------------------------- the map the readers use

test_that("A chunk read through the map is the chunk read from the file", {
  f <- mdFile(mdWav(
    mdFmt(),
    mdBextChunk("A Description"),
    mdChunk("_PMX", mdUtf8("<x:xmpmeta/>")),
    mdChunk("DISP", c(mdU32(1), charToRaw("A Display Name"))),
    mdChunk("DISP", c(mdU32(13), mdUtf16("Another Name"))),
    mdGuanoChunk("Make:Open Acoustic Devices"),
    mdData()
  ))
  map <- .mdWavMap(f)
  expect_s3_class(map, "wavChunkMap")
  for (id in c("fmt ", "bext", "_PMX", "guan")) {
    expect_identical(.mdWavChunk(map, id), .mdWavChunk(f, id), info=id)
  }
  expect_identical(.mdWavChunks(map, "DISP"), .mdWavChunks(f, "DISP"))
  expect_identical(.mdWavChunkIds(map), .mdWavChunkIds(f))
  expect_null(.mdWavChunk(map, "wamd"))
})

test_that("The map answers for the file itself as well as for its chunks", {
  f <- mdFile(mdWav(mdFmt(), mdData(64)))
  map <- .mdWavMap(f)
  expect_identical(.mdFileHead(map, 16L), .mdFileHead(f, 16L))
  expect_identical(.mdFileSize(map), file.size(f))
  expect_identical(.mdFileRange(map, 1L, 4L), charToRaw("RIFF"))
})

test_that("A body the map did not keep still comes back whole", {
  # The audio is never kept, and nor is a chunk too large to be metadata: asking
  # the map for one reads the file, so what comes back cannot be a cached
  # fragment of it.
  f <- mdFile(mdWav(mdFmt(), mdChunk("iXML", mdUtf8("<BWFXML></BWFXML>")), mdData(64)))
  whole <- .mdWavMap(f)
  stingy <- .mdWavMap(f, cap=4L)
  expect_identical(.mdWavChunk(stingy, "iXML"), .mdWavChunk(whole, "iXML"))
  expect_identical(.mdWavChunk(stingy, "iXML"), .mdWavChunk(f, "iXML"))
  expect_identical(length(.mdWavChunk(stingy, "data")), 64L)
})

test_that("A cap smaller than the body is honoured by the map as by the file", {
  f <- mdFile(mdWav(mdFmt(), mdChunk("iXML", mdUtf8("<BWFXML></BWFXML>")), mdData()))
  map <- .mdWavMap(f)
  expect_identical(.mdWavChunk(map, "iXML", cap=6L), .mdWavChunk(f, "iXML", cap=6L))
  expect_identical(length(.mdWavChunk(map, "iXML", cap=6L)), 6L)
})

test_that("A file that is not a wave file has no map, and readers get its path", {
  f <- mdFile(charToRaw("not a wave file"), extension=".mp3")
  expect_null(.mdWavMap(f))
  expect_null(.mdWavMap(file.path(tempdir(), "no-such-file.wav")))
  # A directory exists, so it is opened before it can be refused.
  expect_null(.mdWavMap(tempdir()))
})

test_that("A file holding nothing but a header lists no chunks", {
  # Twelve bytes is a wave file with nothing in it, which is what a recorder
  # interrupted before it wrote anything leaves behind.
  got <- wavChunks(mdFile(mdWav()))
  expect_identical(nrow(got), 0L)
  expect_identical(got$id, character(0))
  expect_output(print(got), "RIFF WAVE, 0 chunks")
})

test_that("A list too short to name its kind names none", {
  # Four bytes of body are the kind of the list. A chunk holding fewer is
  # damaged rather than a list of some kind this does not know.
  f <- mdFile(mdWav(mdFmt(), mdChunk("LIST", charToRaw("IN")), mdData()))
  got <- wavChunks(f)
  expect_identical(got$id, c("fmt ", "LIST", "data"))
  expect_identical(got$form, c(NA_character_, NA_character_, NA_character_))
})

test_that("The map gives the head of a file it did not keep the whole of", {
  # The map keeps the first 64 KiB. Asked for more than it has, it reads the
  # file rather than giving back the short run it holds.
  f <- mdFile(mdWav(mdFmt(), mdData(64)))
  map <- .mdWavMap(f)
  expect_identical(.mdFileHead(map), .mdFileHead(f))
})

test_that("Each reader reads from a map what it reads from the file", {
  f <- mdFile(mdWav(
    mdFmt(),
    mdBextChunk("A Description"),
    mdChunk("iXML", mdUtf8("<BWFXML><PROJECT>A Project</PROJECT></BWFXML>")),
    mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("INAM", "A Title"))),
    mdChunk("_PMX", mdUtf8("<x:xmpmeta><rdf:RDF><rdf:Description dc:format=\"audio/wav\"/></rdf:RDF></x:xmpmeta>")),
    mdChunk("DISP", c(mdU32(1), charToRaw("A Display Name"))),
    mdChunk("wamd", mdWamdField(1L, "A Recorder")),
    mdGuanoChunk("Make:Open Acoustic Devices"),
    mdData()
  ))
  map <- .mdWavMap(f)
  expect_identical(guano(map), guano(f))
  expect_identical(bext(map), bext(f))
  expect_identical(ixml(map), ixml(f))
  expect_identical(wavInfo(map), wavInfo(f))
  expect_identical(xmp(map), xmp(f))
  expect_identical(disp(map), disp(f))
  expect_identical(wamd(map), wamd(f))
  expect_identical(id3(map), id3(f))
})

test_that("readMetadata() reads one walk of the file into the same record", {
  # The record the readers build between them, against the readers called one
  # at a time with the path, which is what readMetadata() used to do.
  f <- mdFile(mdWav(
    mdFmt(),
    mdBextChunk("A Description"),
    mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("IART", "A Recordist"))),
    mdChunk("_PMX", mdUtf8("<x:xmpmeta><dc:title>A Packet Title</dc:title></x:xmpmeta>")),
    mdGuanoChunk("Make:Open Acoustic Devices", "Loc Position:51.5 -0.1"),
    mdData()
  ))
  m <- readMetadata(f)
  expect_identical(m$sources$guano, guano(f))
  expect_identical(m$sources$bext, bext(f))
  expect_identical(m$sources$wavInfo, wavInfo(f))
  expect_identical(m$sources$xmp, xmp(f))
  expect_identical(m$unparsed, .mdUnparsed(.mdWavChunkIds(f)))
  expect_identical(m$container, "WAVE")
})

test_that("One walk is all readMetadata() takes of a wave file", {
  f <- mdFile(mdWav(
    mdFmt(), mdBextChunk("A Description"),
    mdChunk("iXML", mdUtf8("<BWFXML/>")),
    mdChunk("LIST", c(charToRaw("INFO"), mdInfoField("INAM", "A Title"))),
    mdChunk("_PMX", mdUtf8("<x:xmpmeta/>")),
    mdChunk("DISP", c(mdU32(1), charToRaw("A Display Name"))),
    mdGuanoChunk("Make:Open Acoustic Devices"), mdData()
  ))
  opens <- 0L
  trace(base::file, tracer=function() opens <<- opens + 1L, print=FALSE)
  on.exit(untrace(base::file))
  invisible(readMetadata(f))
  untrace(base::file)
  on.exit()
  expect_identical(opens, 1L)
})
