# The TSA corpus, and the collections distributed with it, name a file by its
# collection, the taxon or common name of the animal recorded, a local
# accession number and a hash of the file. For much of that corpus the name is
# the only record of what was recorded, as the files themselves commonly hold
# no embedded metadata whatever.
#
# Every name below is real, taken from the cached corpus, and chosen to cover
# one of the shapes the body of such a name takes.

test_that("A TSA filename is recognised by the hash it ends with", {
  expect_identical(
    .detectFormat("TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav"),
    "TSA"
  )
  # The collections are many and their accession numbers follow no one layout,
  # so neither can be matched on; the hash is the part every name shares.
  expect_null(.detectFormat("TSA-Porzana_parva_DIG_104_26_1.wav"))
  expect_null(.detectFormat("TSA-Porzana_parva-nothexadecimal0-.wav"))
})

test_that("A TSA filename gives its collection, taxon and accession number", {
  p <- parseFilename("TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav")

  expect_identical(p$match, "TSA")
  expect_identical(p$collection, "TSA")
  expect_identical(p$taxon, "Porzana parva")
  expect_identical(p$accession, "DIG_104_26_1")
  expect_identical(p$hash, "ee281ac7afd44e08")
})

# These names hold no time of any kind, so the field every other format fills
# is reported as missing rather than left out, which would change the shape of
# the result from one filename to the next.
test_that("A TSA filename reports no time, and is not a guess", {
  p <- parseFilename("FRA-Picus_canus_PF02729-ea298bcddfc8fd49.wav")

  expect_true("datetime" %in% names(p))
  expect_true(is.na(p$datetime))
  expect_false("ambiguous" %in% names(p))
})

test_that("Every collection in the corpus is read the same way", {
  collections <- list(
    "FRA-Picus_canus_PF02729-ea298bcddfc8fd49.wav" = c("FRA", "Picus canus"),
    "SUB-Streptopelia-decaocto_Sub_CD0019_29-ec1dc671177836cb.wav" =
      c("SUB", "Streptopelia decaocto"),
    "J_Orn-Anthus_campestris_149463-6836e6c51570d073.wav" =
      c("J_Orn", "Anthus campestris"),
    "FAB-Acheta_domestica_Arth_199_11_1-cfb74bf3f237135d.wav" =
      c("FAB", "Acheta domestica"),
    "VN-Parus_major_Mar_00003792-1fde00c05ebc317e.wav" = c("VN", "Parus major")
  )
  for (name in names(collections)) {
    p <- parseFilename(name)
    expect_identical(p$collection, collections[[name]][1])
    expect_identical(p$taxon, collections[[name]][2])
  }
})

# The collection name itself may hold an underscore, so the parts are split on
# the hyphens rather than on the underscores.
test_that("A collection whose name holds an underscore is read whole", {
  p <- parseFilename("J_Orn-Anthus_campestris_149463-6836e6c51570d073.wav")

  expect_identical(p$collection, "J_Orn")
  expect_identical(p$accession, "149463")
})

# One collection joins the genus and the species with a hyphen where the rest
# use an underscore.
test_that("A name joined by a hyphen gives the same taxon", {
  p <- parseFilename("SUB-Streptopelia-decaocto_Sub_CD0019_29-ec1dc671177836cb.wav")

  expect_identical(p$taxon, "Streptopelia decaocto")
  expect_identical(p$accession, "Sub_CD0019_29")
})

test_that("A species that was not identified is read as written", {
  p <- parseFilename("SUB-Calidris_spec._Sub_CD0032_09-3f914b3408fc5142.wav")

  expect_identical(p$taxon, "Calidris spec.")
  expect_identical(p$accession, "Sub_CD0032_09")
})

test_that("An accession number that encodes an extract is kept whole", {
  p <- parseFilename(
    "TSA-Myotis_myotis_Ski0112_S1_From0586218ms_To0605545ms-7b1bdf160d5a775a.wav"
  )

  expect_identical(p$taxon, "Myotis myotis")
  expect_identical(p$accession, "Ski0112_S1_From0586218ms_To0605545ms")
})

test_that("A taxon with nothing after it gives no accession number", {
  p <- parseFilename("TSA-Parus_major-1fde00c05ebc317e.wav")

  expect_identical(p$taxon, "Parus major")
  expect_true(is.na(p$accession))
})

# Part of the corpus is named by a German common name and a German word for the
# kind of call. Reading those would need a German lexicon, and reporting the
# first words of such a name as a genus and a species would invent a taxon the
# file was never given, so no taxon is read at all and the name is kept whole.
test_that("A name that is not a scientific name gives no taxon", {
  german <- c(
    "TSA-1668_Bachstelze_Flugrufe-870d2e55c47158ae.wav",
    "TSA-0929_Orang_Utan_Jungtier-e49416f5953bb345.wav",
    "TSA-1015_Riesensturmvogel_Rufe_am_Brutplatz-9daf60cc8afd36eb.wav",
    "SP-ac_SP0026_RTJ223-37ee4a4ddd8f7c0f.wav"
  )
  for (name in german) {
    p <- parseFilename(name)
    expect_identical(p$match, "TSA")
    expect_true(is.na(p$taxon))
    expect_false(is.na(p$accession))
  }

  expect_identical(
    parseFilename("TSA-1668_Bachstelze_Flugrufe-870d2e55c47158ae.wav")$accession,
    "1668_Bachstelze_Flugrufe"
  )
})

test_that("A list of these names is read a name at a time", {
  parsed <- parseFilename(list(
    "TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav",
    "FRA-Picus_canus_PF02729-ea298bcddfc8fd49.wav"
  ))

  expect_length(parsed, 2)
  expect_identical(parsed[[1]]$taxon, "Porzana parva")
  expect_identical(parsed[[2]]$taxon, "Picus canus")
})

test_that("The format can be asked for by name", {
  p <- parseFilename(
    "TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav", format="TSA"
  )

  expect_identical(p$taxon, "Porzana parva")
  expect_error(
    parseFilename("20240220_162231.wav", format="TSA"),
    "as a TSA filename"
  )
})

test_that("TSA is one of the formats the package says it knows", {
  expect_true("TSA" %in% .knownFileFormats())
})
