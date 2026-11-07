#' Run a pipeline over a set of recordings
#'
#' Runs a [pipeline()] on every file of a [RecordingSet][recordingSet()] and
#' writes its results to a folder, the store. The store is the whole state of a
#' run, so the same call made again carries on where the last one stopped, and
#' the results are read back with [pipelineResults()].
#'
#' @section Files:
#' The recordings of a set that share a hash or a location are one file, as a
#' recording the Animal Sound Archive lists under three species is, and each
#' file is run once. A file is fetched from its location: a local file is read
#' where it is, and one at a URL is downloaded, then deleted once it has been
#' run unless `keep` names a folder to keep it in. Each file is read by
#' [readAudio()], whole unless it is too long to hold (see Long files), tagged
#' with its location as its origin and with the columns
#' of the set that all of its recordings agree on, such as `start`, `lat` and
#' `lon`, in the `"recordingSet"` entry of its metadata, and measured by
#' [calculateMetadata()]. Columns its recordings disagree on, such as the
#' `taxon` of a file listed under three species, are left in the set.
#'
#' A file is known by the SHA-256 hash of its bytes. A file the set gives a hash
#' has to match it, and a local file the set gives none is hashed before the run
#' starts, so that a file already in the store is recognised wherever it has
#' been moved to, and copies of one file are run once. A file that cannot be
#' hashed then is left to fail when it is read. A file at a URL the set gives no
#' hash is recognised by its location once it has been run. A local location is
#' made absolute before the run, as a worker of a cluster can be working in
#' another folder.
#'
#' @section Long files:
#' A file is read whole unless it would take more than `max.memory` bytes once
#' decoded, which is worked out from its header before it is read. R holds a
#' sample in 4 bytes for a WAV file of 8, 16 or 32-bit PCM and for an MP3 file,
#' and in 8 for a WAV file of 24-bit PCM or floating-point audio and for
#' anything the av package decodes, FLAC among them.
#'
#' A file over `max.memory` is read a window at a time, by each [windowing()]
#' at the top of the pipeline, as windowing() reads a file. Each window holds
#' the samples, and the tags, of the window cut from the whole recording, so
#' the results are those a whole read gives, and the `wave_hash` of the file is
#' taken a block at a time. That writes the samples to a temporary file, as the
#' digest is of all of them at once, so it needs room on disk for 8 bytes a
#' sample while it is taken. Only a RIFF WAV file of PCM or floating-point audio
#' can be read in parts that are exact, so any other file over `max.memory`, an
#' MP3 or a FLAC file among them, fails without being read. So does a file whose
#' pipeline has a step outside a windowing given `then=`, which needs the whole
#' recording at once, or windows that are themselves over `max.memory`. The
#' size of a file other than a WAV file is asked of the av package, and where
#' that is not installed such a file is read whole.
#'
#' @section Batches:
#' The files not yet in the store are run in batches of `batch` files, and each
#' batch is written to the store once all of its files have been run. Stopping a
#' run loses only the batch it was in, and the next run starts again from that
#' batch. With `cl`, a cluster from the parallel package, each worker runs whole
#' batches and writes them itself, and needs this package installed.
#'
#' @section Failures:
#' A file that cannot be fetched or read, that does not match the hash the set
#' gives it, or on which a step fails, is recorded as failed with the reason,
#' and the run moves on. It has a row in every table of results, with `error`
#' saying what went wrong, and it is not run again unless `retry` is `TRUE`.
#'
#' @section The store:
#' The store holds a folder for each pipeline run into it, named by the
#' pipeline's id, so pipelines that share a store are kept apart and a file is
#' run once for each of them. That folder holds the pipeline itself, in
#' `pipeline.rds`, and a `batches` folder with a list of the files each batch
#' covered and a file for each of its tables of results, so that one table can
#' be read without the others. As the id covers the code of every package a
#' step uses, a pipeline run after any of them has changed is run into a folder
#' of its own.
#'
#' @param p A `Pipeline`.
#' @param set A `RecordingSet`.
#' @param store The folder to write the results to. It is made if it does not
#'   exist.
#' @param batch How many files to run between writes to the store.
#' @param max.memory The most memory, in bytes, a file may take once decoded to
#'   be read whole, 4 GB unless it is given, or `Inf` to read every file whole.
#' @param keep A folder to keep downloaded files in, or `NULL` to delete each
#'   one once it has been run. Each file is kept in a folder of its own, named
#'   from its URL, so that files of the same name from different places are
#'   kept apart, and one already there is not downloaded again.
#' @param retry If `TRUE`, files that failed in an earlier run are run again.
#' @param cl A cluster from the parallel package, to run batches on, or `NULL`.
#' @param quiet If `TRUE`, no progress is reported.
#' @return The set, invisibly, with the hash of every file the store holds
#'   filled in where the set gave none, so that its rows can be joined to the
#'   results by `hash`.
#' @seealso [pipeline()] to write one, and [pipelineResults()] to read what a
#'   run wrote.
#' @export
#' @examples
#' \dontrun{
#' # Working out a pipeline's id hashes the code of every package it uses,
#' # which takes some seconds.
#' f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
#' set <- recordingSet(f, source="AudioMoth")
#' p <- pipeline(windowing(1, time.unit="seconds", then=dutyCycle()))
#' store <- file.path(tempdir(), "store")
#' set <- runPipeline(p, set, store)
#' res <- pipelineResults(store, p)
#' head(res[[1]])
#' merge(res[[1]], set, by.x="file", by.y="hash")[, c("window_start", "value", "uri")]
#' }
runPipeline <- function(p, set, store, batch=1000, max.memory=4e9, keep=NULL, retry=FALSE,
                        cl=NULL, quiet=FALSE) {
  if (!inherits(p, "Pipeline")) {
    stop("runPipeline() runs a Pipeline, not ", .aDescription(p), ".")
  }
  if (!inherits(set, "RecordingSet")) {
    stop("runPipeline() runs over a RecordingSet, not ", .aDescription(set),
         ". recordingSet() makes one.")
  }
  #A subset keeps the class while it has a uri and a location, and is given
  #back the columns every set has, such as hash, that it was cut down without.
  set <- recordingSet(set)
  .validateFolder(store, "store")
  if (!is.numeric(batch) || length(batch) != 1 || is.na(batch) || batch < 1 ||
      batch != round(batch)) {
    stop("batch must be a whole number of files, at least 1.")
  }
  if (!is.numeric(max.memory) || length(max.memory) != 1 || is.na(max.memory) ||
      max.memory <= 0) {
    stop("max.memory must be a number of bytes greater than 0, or Inf.")
  }
  if (!is.null(keep)) {
    .validateFolder(keep, "keep")
  }
  if (!isTRUE(retry) && !isFALSE(retry)) {
    stop("retry must be TRUE or FALSE.")
  }
  #Made absolute, as a worker of a cluster can be working in another folder.
  store <- .madeFolder(store)
  if (!is.null(keep)) {
    keep <- .madeFolder(keep)
  }
  identity <- .pipelineIdentity(p)
  dir <- .pipelineStore(store, p, identity)
  work <- .absoluteLocations(set)
  group <- .recordingSetFiles(work)
  records <- .storeRecords(.readManifests(dir))
  #The hashes found before the run go into the set, which is grouped again, so
  #that recordings found to be of the same file are run once, and the columns
  #they agree on are agreed across all of them.
  found <- .knownHashes(.setFiles(work, group), records)
  unhashed <- is.na(work$hash)
  work$hash[unhashed] <- found[group][unhashed]
  group <- .recordingSetFiles(work)
  files <- .setFiles(work, group)
  todo <- files[.toRun(files, .latestFiles(records), retry), , drop=FALSE]
  if (!quiet) {
    message(
      "Pipeline ", substr(identity$id, 1, 8), ": ", nrow(todo),
      if (nrow(todo) == 1) " file" else " files", " to run, ",
      nrow(files) - nrow(todo), " already in the store."
    )
  }
  jobs <- .batchJobs(todo, work, group, batch, .nextBatch(dir))
  if (is.null(cl)) {
    for (i in seq_along(jobs)) {
      ran <- .runPipelineBatch(jobs[[i]], p, identity$id, dir, keep, max.memory)
      if (!quiet) {
        message(.batchReport(ran, paste("Batch", i, "of", length(jobs))))
      }
    }
  } else if (length(jobs) > 0) {
    ran <- parallel::parLapply(cl, jobs, .runPipelineBatch, p=p, id=identity$id,
                               dir=dir, keep=keep, max.memory=max.memory)
    if (!quiet) {
      message(.batchReport(do.call(rbind, ran), paste(
        length(jobs), if (length(jobs) == 1) "batch" else "batches"
      )))
    }
  }
  #The hashes found before the run come first, as a file that has been moved is
  #in the store under its old location only. The rest are found by the absolute
  #locations the store records, and only the hashes go back to the set, whose
  #locations are left as they were given.
  set$hash <- .withHashes(work, .storeRecords(.readManifests(dir)))$hash
  return(invisible(set))
}

#' Read the results of a pipeline from a store
#'
#' @description
#' Reads what [runPipeline()] wrote: a table for each analysis step of the
#' pipeline, named by the step's path through it, as
#' `windowing(60, time.unit = "seconds") / maad_temporal_entropy()`, in the order
#' of its outline. Each table holds one measurement at one grain, so values from
#' windows of different lengths, or from raw and filtered audio, are never in
#' the same table.
#'
#' @details
#' Every table has these columns, in this order:
#'
#' * `file`: the SHA-256 hash of the file the row was measured on, which joins
#'   to the `hash` of the set that [runPipeline()] gives back.
#' * `pipeline`: the id of the pipeline.
#' * `cut_start`, `cut_end`, `window_start`, `window_end`, `window_length`:
#'   where in the recording the row was measured, in seconds from its start,
#'   for a step inside a cut or a windowing, as [pipeline()] describes.
#' * `window_time`: when the window began, where all the recordings of the file
#'   give the same `start`.
#' * `error`: why the file failed, or `NA`.
#' * `value` and the columns beginning `value.`: the result, as
#'   [flattenOutput()] gives it.
#'
#' A file that failed has one row in every table, holding only its `file`,
#' `pipeline` and `error`. A file in which a step found nothing has no rows in
#' its table, as [flattenOutput()] gives none for `NULL`, and the `"files"`
#' attribute tells such a file apart from one that was never run. A table is
#' listed once a file has run without failing.
#'
#' A file that was run more than once, by `retry = TRUE`, gives the rows of its
#' last run that did not fail, or its last error where every run failed. A file
#' at a URL that failed before its hash was known is matched to its hash once a
#' later run finds it.
#'
#' @param store The folder [runPipeline()] wrote to.
#' @param p The `Pipeline`, or its id. The id changes with the code of the
#'   packages a pipeline uses, so results written before a package was updated
#'   are read with the id they were written under, which is the name of their
#'   folder in the store.
#' @param step `NULL` for every table, or the name of one, or its number in the
#'   outline of the pipeline, as `"2.1"`, to read that table alone.
#' @return A named list of data frames, one for each analysis step, or a single
#'   data frame where `step` is given. The list has a `"files"` attribute, a
#'   data frame of every file the store holds: its `file` hash, the `uri` of its
#'   first recording, its `location`, its `status`, `"ok"` or `"error"`, its
#'   `error`, the `time` it was run and the `batch` it was run in.
#' @seealso [runPipeline()]
#' @export
pipelineResults <- function(store, p, step=NULL) {
  .validateFolder(store, "store")
  id <- .resultsId(store, p)
  dir <- file.path(store, id)
  outline <- .outline(readRDS(file.path(dir, "pipeline.rds"))$pipeline)
  manifests <- .readManifests(dir)
  files <- .latestFiles(.storeRecords(manifests))
  #Named from the pipeline the store was made with: pipelines that write their
  #calls differently, as dutyCycle(0.2) and dutyCycle(limit = 0.2), share an id
  #and so a folder, but not the names of their tables.
  held <- unlist(lapply(manifests, function(m) m$tables$number))
  tables <- outline[outline$number %in% held, c("number", "name"), drop=FALSE]
  if (!is.null(step)) {
    if (!is.character(step) || length(step) != 1 || is.na(step)) {
      stop("step must be the name of one table, or its number in the outline.")
    }
    found <- which(tables$name == step | tables$number == step)
    if (length(found) == 0) {
      stop("The store holds no table called ", step, ". It holds: ",
           if (nrow(tables) == 0) "none" else paste(tables$name, collapse=", "), ".")
    }
    return(.readTable(tables$number[found], dir, id, manifests, files))
  }
  out <- lapply(tables$number, .readTable, dir=dir, id=id, manifests=manifests, files=files)
  names(out) <- tables$name
  files$observed <- NULL
  attr(out, "files") <- files
  return(out)
}

#' Check that something names a folder
#' @param x The value.
#' @param what The name of the argument, for the error.
#' @keywords internal
#' @noRd
.validateFolder <- function(x, what) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    stop(what, " must be the path of a folder.")
  }
}

#' Make a folder, and give its absolute path
#' @param path The folder.
#' @return Its absolute path.
#' @keywords internal
#' @noRd
.madeFolder <- function(path) {
  dir.create(path, recursive=TRUE, showWarnings=FALSE)
  if (!dir.exists(path)) {
    stop("Could not make the folder ", path, ".")
  }
  return(normalizePath(path, winslash="/"))
}

#' Is a location a URL?
#' @param x Locations.
#' @return A logical vector, FALSE for NA.
#' @keywords internal
#' @noRd
.isUrl <- function(x) {
  return(!is.na(x) & grepl("^https?://", x, ignore.case=TRUE))
}

#' A set with its local locations made absolute
#'
#' A set made from a data frame keeps a location as it was given, and a worker
#' of a cluster can be working in another folder, where a relative path names
#' another file or none.
#' @param set A RecordingSet.
#' @return The set.
#' @keywords internal
#' @noRd
.absoluteLocations <- function(set) {
  local <- !is.na(set$location) & !.isUrl(set$location)
  set$location[local] <- .absolutePath(set$location[local])
  return(set)
}

#' The absolute path of a file, whether or not it exists yet
#'
#' normalizePath() leaves a file that does not exist as it was given, which on
#' Unix can be relative and on Windows can hold the short name of a folder,
#' as RUNNER~1, that it gives in full once the file is there. Such a file is
#' found through the deepest folder of its path that does exist, so that its
#' path is the same before it exists as after, and a run that failed for want
#' of it is matched to the file once it is there.
#' @param path Paths.
#' @return Absolute paths, with "/" between folders.
#' @keywords internal
#' @noRd
.absolutePath <- function(path) {
  out <- path
  there <- file.exists(path)
  out[there] <- normalizePath(path[there], winslash="/")
  out[!there] <- vapply(path.expand(path[!there]), function(p) {
    if (!grepl("^([A-Za-z]:)?[/\\\\]", p)) {
      p <- file.path(getwd(), p)
    }
    rest <- character(0)
    while (!file.exists(p) && dirname(p) != p) {
      rest <- c(basename(p), rest)
      p <- dirname(p)
    }
    #The root of a drive ends in "/", as other folders do not. A file is below
    #a folder, so something is always added after it.
    found <- sub("/+$", "", normalizePath(p, winslash="/", mustWork=FALSE))
    return(paste(c(found, rest), collapse="/"))
  }, character(1), USE.NAMES=FALSE)
  return(out)
}

#' Write an object to a file in one step
#'
#' It is written to a file beside the one it is for and renamed into place, so a
#' run stopped part way through leaves the whole file or none, never a truncated
#' one that could not be read again. The name the file is written under first
#' matches none of the names the store reads.
#' @param object The object.
#' @param path The file.
#' @return `path`, invisibly.
#' @keywords internal
#' @noRd
.saveWhole <- function(object, path) {
  part <- tempfile(paste0(basename(path), "."), tmpdir=dirname(path), fileext=".part")
  on.exit(unlink(part))
  saveRDS(object, part)
  if (!suppressWarnings(file.rename(part, path))) {
    stop("Could not write ", path, ".")
  }
  return(invisible(path))
}

#' The folder of a pipeline in a store
#'
#' Made with the pipeline in it the first time the pipeline is run into the
#' store, so that its results can be read, and its outline printed, without it.
#' @param store The store.
#' @param p The pipeline.
#' @param identity Its id and code, as `.pipelineIdentity()` gives them.
#' @return The path of the folder.
#' @keywords internal
#' @noRd
.pipelineStore <- function(store, p, identity) {
  dir <- file.path(store, identity$id)
  dir.create(file.path(dir, "batches"), recursive=TRUE, showWarnings=FALSE)
  record <- file.path(dir, "pipeline.rds")
  if (!file.exists(record)) {
    .saveWhole(list(
      pipeline=p, id=identity$id, code=identity$code,
      created=structure(Sys.time(), tzone="UTC"), R=R.version.string
    ), record)
  }
  return(dir)
}

#' The files of a set
#'
#' A file is given the hash its recordings give it, the first location they
#' give, and the uri of its first recording.
#' @param set A RecordingSet.
#' @param group The file each recording is in, as `.recordingSetFiles()` gives.
#' @return A data frame with one row per file: `file`, its number in `group`,
#'   `hash`, `uri`, `location`, and `problem`, why the file cannot be run, or
#'   `NA`.
#' @keywords internal
#' @noRd
.setFiles <- function(set, group) {
  n <- if (length(group) == 0) 0L else max(group)
  hashed <- which(!is.na(set$hash))
  pairs <- unique(data.frame(group=group[hashed], hash=set$hash[hashed],
                             stringsAsFactors=FALSE))
  count <- tabulate(pairs$group, n)
  hash <- rep(NA_character_, n)
  single <- pairs[count[pairs$group] == 1, , drop=FALSE]
  hash[single$group] <- single$hash
  first <- which(!duplicated(group))
  uri <- rep(NA_character_, n)
  uri[group[first]] <- set$uri[first]
  located <- which(!is.na(set$location))
  first <- located[!duplicated(group[located])]
  location <- rep(NA_character_, n)
  location[group[first]] <- set$location[first]
  problem <- rep(NA_character_, n)
  problem[count > 1] <- paste0(
    "The set gives this file ", count[count > 1], " different hashes."
  )
  problem[is.na(location)] <- "The set gives no location for this file."
  return(data.frame(file=seq_len(n), hash=hash, uri=uri, location=location,
                    problem=problem, stringsAsFactors=FALSE))
}

#' The hash of each file, as far as it can be known before the run
#'
#' A file at a URL the set gives no hash is given the one the store has for its
#' location, and a local file that exists is hashed. A local file that cannot be
#' read to hash it is left unknown, to fail when it is read, rather than stopping
#' the run for every file.
#' @param files The files, as `.setFiles()` gives them.
#' @param records Every record of the store, as `.storeRecords()` gives them.
#' @return The hash of each file, `NA` where it is not known.
#' @keywords internal
#' @noRd
.knownHashes <- function(files, records) {
  hash <- files$hash
  unknown <- is.na(hash) & is.na(files$problem)
  url <- unknown & .isUrl(files$location)
  hash[url] <- .hashAt(files$location[url], records)
  local <- which(unknown & !url & file.exists(files$location) & !dir.exists(files$location))
  hash[local] <- vapply(files$location[local], function(path) {
    tryCatch(.audioblast_sha256(path), error=function(e) NA_character_)
  }, character(1), USE.NAMES=FALSE)
  return(hash)
}

#' What a file is known by in a store
#'
#' Its hash, or where that is not known, its location, or where neither is, the
#' uri of its first recording.
#' @param file,location,uri Of each file.
#' @return A character vector.
#' @keywords internal
#' @noRd
.fileKey <- function(file, location, uri) {
  return(ifelse(
    !is.na(file), file,
    ifelse(!is.na(location), paste0("location:", location), paste0("uri:", uri))
  ))
}

#' Which files a run has to run
#'
#' Every file the store holds no record of, and with `retry` every file whose
#' last run failed. A file that failed before its hash was known, as a local
#' file that was not there yet does, is in the store under its location, and
#' is found there once it has been hashed, so that it is not run again without
#' `retry`. Only failures are kept without a hash, so a location matches no
#' file that ran.
#' @param files The files, with the hashes `.knownHashes()` gives.
#' @param stored The files of the store.
#' @param retry Whether to run failed files again.
#' @return A logical vector, one for each file.
#' @keywords internal
#' @noRd
.toRun <- function(files, stored, retry) {
  keys <- .fileKey(stored$file, stored$location, stored$uri)
  found <- match(.fileKey(files$hash, files$location, files$uri), keys)
  unmatched <- which(is.na(found) & !is.na(files$hash))
  found[unmatched] <- match(.fileKey(
    rep(NA_character_, length(unmatched)), files$location[unmatched], files$uri[unmatched]
  ), keys)
  status <- stored$status[found]
  return(is.na(status) | (retry & status == "error"))
}

#' Split the files to run into batches
#' @param todo The files to run.
#' @param set The set, whose rows of each file go with it.
#' @param group The file each of its recordings is in.
#' @param size How many files to a batch.
#' @param first The number of the first batch.
#' @return A list of batches, each a list of its `number`, its `files` and the
#'   `rows` of the set they are, with the file each row is in as `.file`.
#' @keywords internal
#' @noRd
.batchJobs <- function(todo, set, group, size, first) {
  if (nrow(todo) == 0) {
    return(list())
  }
  rows <- set
  class(rows) <- "data.frame"
  rows$.file <- group
  #The rows of each file, found once rather than by searching the whole set for
  #the files of every batch.
  of_file <- split(seq_len(nrow(rows)), factor(group, levels=seq_len(max(group))))
  chunks <- split(seq_len(nrow(todo)), ceiling(seq_len(nrow(todo)) / size))
  return(unname(lapply(seq_along(chunks), function(i) {
    files <- todo[chunks[[i]], , drop=FALSE]
    list(
      number=first + i - 1,
      files=files,
      rows=rows[unlist(of_file[files$file], use.names=FALSE), , drop=FALSE]
    )
  })))
}

#' Run one batch and write it to the store
#'
#' Its tables are written first, and the list of the files it covered last, so
#' a batch stopped part way through has no list, and is neither read nor taken
#' to have been run. Each is written whole or not at all.
#'
#' Two files of the set can turn out to hold the same bytes once they are
#' hashed, as two URLs the set gives no hash for can. Each is recorded, so that
#' both locations are known to hold that hash, and the rows kept are those of
#' the first of them to run without failing.
#' @param job A batch, as `.batchJobs()` gives it.
#' @param p The pipeline.
#' @param id Its id.
#' @param dir Its folder in the store.
#' @param keep The folder to keep downloads in, or `NULL`.
#' @param max.memory The most a file may take decoded to be read whole.
#' @return The records of the files it ran, as written to the store.
#' @keywords internal
#' @noRd
.runPipelineBatch <- function(job, p, id, dir, keep, max.memory=Inf) {
  outline <- .outline(p)
  numbers <- stats::setNames(outline$number, outline$name)
  records <- list()
  tables <- list()
  measured <- character(0)
  for (i in seq_len(nrow(job$files))) {
    f <- job$files[i, , drop=FALSE]
    ran <- .runPipelineFile(p, id, f, job$rows[job$rows$.file == f$file, , drop=FALSE], keep,
                            max.memory)
    records[[length(records) + 1]] <- ran$record
    if (ran$record$status != "ok" || ran$record$file %in% measured) {
      next
    }
    measured <- c(measured, ran$record$file)
    for (name in names(ran$results)) {
      tables[[name]] <- c(tables[[name]], list(ran$results[[name]]))
    }
  }
  for (name in names(tables)) {
    .saveWhole(.bindResults(tables[[name]]), .tablePath(dir, job$number, numbers[[name]]))
  }
  files <- do.call(rbind, lapply(records, as.data.frame, stringsAsFactors=FALSE))
  files$batch <- rep(job$number, nrow(files))
  .saveWhole(list(
    batch=job$number,
    files=files,
    tables=data.frame(number=unname(numbers[names(tables)]), name=names(tables),
                      stringsAsFactors=FALSE)
  ), .manifestPath(dir, job$number))
  return(files)
}

#' Run the pipeline on one file
#' @param p The pipeline.
#' @param id Its id.
#' @param f The file, a row of `.setFiles()`.
#' @param rows The rows of the set that are this file.
#' @param keep The folder to keep downloads in, or `NULL`.
#' @param max.memory The most the file may take decoded to be read whole. A
#'   file over it is read a window at a time where it can be, and otherwise
#'   fails before it is read.
#' @return A list of the file's `record` and its `results`, the rows of each
#'   table it gave.
#' @keywords internal
#' @noRd
.runPipelineFile <- function(p, id, f, rows, keep, max.memory=Inf) {
  #Until the file has been read, its hash is the one the set gives, which can
  #be wrong.
  failed <- function(e, hash=f$hash, observed=FALSE) {
    return(list(
      record=.fileRecord(hash, f$uri, f$location, conditionMessage(e), observed),
      results=list()
    ))
  }
  if (!is.na(f$problem)) {
    return(failed(simpleError(f$problem)))
  }
  fetched <- tryCatch(.fetchFile(f$location, rows, keep), error=function(e) e)
  if (inherits(fetched, "error")) {
    return(failed(fetched))
  }
  if (!is.null(fetched$temporary)) {
    on.exit(unlink(fetched$temporary, recursive=TRUE), add=TRUE)
  }
  metadata <- .agreedColumns(rows)
  #Nothing is worked out of a file's size where every file is read whole, and
  #a file whose size cannot be told is read whole, for readAudio() to say what
  #is wrong with it.
  size <- if (is.infinite(max.memory)) NULL else {
    tryCatch(.decodedSize(fetched$path), error=function(e) NULL)
  }
  over <- !is.null(size) && size$bytes > max.memory
  if (over) {
    problem <- .longFileProblem(p, size, max.memory)
    if (!is.null(problem)) {
      return(failed(simpleError(problem)))
    }
  }
  long <- NULL
  w <- tryCatch({
    if (over) {
      long <- .longFileWave(fetched$path, f$location, metadata)
      long$wave
    } else {
      tw <- tagWave(readAudio(fetched$path), origin=f$location)
      tw@metadata[["recordingSet"]] <- metadata
      calculateMetadata(tw)
    }
  }, error=function(e) e)
  if (!is.null(long$file$copy)) {
    on.exit(unlink(long$file$copy), add=TRUE)
  }
  if (inherits(w, "error")) {
    return(failed(w))
  }
  hash <- w@metadata[["calculatedMetadata"]]$hash
  if (!is.na(f$hash) && hash != f$hash) {
    return(failed(simpleError(paste0(
      "The file at ", f$location, " has the hash ", hash, ", not the ", f$hash,
      " the set gives it."
    ))))
  }
  ran <- tryCatch(.runPipelineWave(p, w, long$file)$results, error=function(e) e)
  if (inherits(ran, "error")) {
    return(failed(ran, hash, observed=TRUE))
  }
  #Exactly, as $ would give a column such as start_offset where the recordings
  #do not agree on start.
  return(list(
    record=.fileRecord(hash, f$uri, f$location, NA_character_, observed=TRUE),
    results=lapply(ran, .keyedRows, file=hash, id=id, start=metadata[["start"]])
  ))
}

#' What the store records of a file
#' @param hash Its hash, `NA` where it is not known.
#' @param uri The uri of its first recording.
#' @param location Where it was fetched from.
#' @param error Why it failed, or `NA`.
#' @param observed Whether `hash` was read from the file's bytes, rather than
#'   taken from the set, so that only a hash the file was found to have is
#'   taken to be the hash of what is at its location.
#' @return A list.
#' @keywords internal
#' @noRd
.fileRecord <- function(hash, uri, location, error, observed) {
  return(list(
    file=hash, uri=uri, location=location, status=if (is.na(error)) "ok" else "error",
    error=error, time=structure(Sys.time(), tzone="UTC"), observed=observed
  ))
}

#' Fetch a file
#'
#' A local file is read where it is. A file at a URL is downloaded into a folder
#' of its own: within the folder `keep` names, one named from the start of the
#' SHA-256 hash of its URL, where one already there is used as it is, or
#' otherwise a temporary folder, to be deleted once the file has been run. It is
#' named as [audioblastDownload()] names it, which can give two URLs the same
#' name, so the folder is what tells their files apart.
#' @param location Where the file is.
#' @param rows The rows of the set that are the file.
#' @param keep The folder to keep downloads in, or `NULL`.
#' @return A list of the `path` to read it from and the `temporary` folder to
#'   delete afterwards, or `NULL`.
#' @keywords internal
#' @noRd
.fetchFile <- function(location, rows, keep) {
  if (!.isUrl(location)) {
    if (!file.exists(location) || dir.exists(location)) {
      stop("There is no file at ", location, ".")
    }
    return(list(path=location, temporary=NULL))
  }
  temporary <- if (is.null(keep)) tempfile("runPipeline")
  folder <- if (is.null(keep)) temporary else file.path(
    keep, substr(digest::digest(location, algo="sha256", serialize=FALSE), 1, 16)
  )
  dir.create(folder, recursive=TRUE, showWarnings=FALSE)
  path <- file.path(folder, .audioblast_file_names(rows[1, , drop=FALSE], location))
  if (!file.exists(path)) {
    tryCatch(.audioblast_download(location, path), error=function(e) {
      if (!is.null(temporary)) {
        unlink(temporary, recursive=TRUE)
      }
      stop("Could not download ", location, ": ", conditionMessage(e), call.=FALSE)
    })
  }
  return(list(path=path, temporary=temporary))
}

#' The columns all the recordings of a file agree on
#'
#' A value that is not known in any of them is left out, as is one they
#' disagree on.
#' @param rows The rows of the set that are one file.
#' @return A named list of single values.
#' @keywords internal
#' @noRd
.agreedColumns <- function(rows) {
  rows$.file <- NULL
  agreed <- vapply(rows, function(v) {
    is.atomic(v) && !is.na(v[1]) && all(!is.na(v) & v == v[1])
  }, logical(1))
  return(lapply(rows[agreed], `[`, 1))
}

#' The rows of one table from one file, with their keys in front
#'
#' The file and the pipeline, then where in the recording each row was
#' measured, the clock time of a window where the start of the recording is
#' known, the error, which is `NA` for a file that ran, and then the result.
#' @param rows A table as the run gives it.
#' @param file The hash of the file.
#' @param id The id of the pipeline.
#' @param start When the recording began, or `NULL`.
#' @return A data frame.
#' @keywords internal
#' @noRd
.keyedRows <- function(rows, file, id, start) {
  n <- nrow(rows)
  values <- startsWith(names(rows), "value")
  cols <- c(list(file=rep(file, n), pipeline=rep(id, n)), as.list(rows)[!values])
  if (!is.null(start) && "window_start" %in% names(rows)) {
    cols$window_time <- start + rows$window_start
  }
  cols$error <- rep(NA_character_, n)
  return(structure(
    c(cols, as.list(rows)[values]), class="data.frame", row.names=.set_row_names(n)
  ))
}

#' The path of the list of the files a batch covered
#' @param dir The folder of a pipeline in a store.
#' @param batch The number of the batch.
#' @keywords internal
#' @noRd
.manifestPath <- function(dir, batch) {
  return(file.path(dir, "batches", sprintf("%06d.rds", batch)))
}

#' The path of one table of a batch
#' @param dir The folder of a pipeline in a store.
#' @param batch The number of the batch.
#' @param number The number of the table's step in the outline.
#' @keywords internal
#' @noRd
.tablePath <- function(dir, batch, number) {
  return(file.path(dir, "batches", sprintf("%06d_%s.rds", batch, number)))
}

#' The lists of the files each batch covered, in the order they were written
#' @param dir The folder of a pipeline in a store.
#' @return A list of what `.runPipelineBatch()` wrote of each batch.
#' @keywords internal
#' @noRd
.readManifests <- function(dir) {
  found <- list.files(file.path(dir, "batches"), pattern="^[0-9]+\\.rds$", full.names=TRUE)
  found <- found[order(as.numeric(sub("\\.rds$", "", basename(found))))]
  return(lapply(found, readRDS))
}

#' The number the next batch written to a store takes
#' @param dir The folder of a pipeline in a store.
#' @return A number.
#' @keywords internal
#' @noRd
.nextBatch <- function(dir) {
  found <- list.files(file.path(dir, "batches"), pattern="^[0-9]+\\.rds$")
  return(if (length(found) == 0) 1 else max(as.numeric(sub("\\.rds$", "", found))) + 1)
}

#' Every record of a store, in the order the batches were written
#' @param manifests What `.readManifests()` gives.
#' @return A data frame of `file`, `uri`, `location`, `status`, `error`, `time`,
#'   `observed` and `batch`, one row for each time a file was run.
#' @keywords internal
#' @noRd
.storeRecords <- function(manifests) {
  return(do.call(rbind, c(
    list(data.frame(
      file=character(0), uri=character(0), location=character(0),
      status=character(0), error=character(0), time=.POSIXct(numeric(0), tz="UTC"),
      observed=logical(0), batch=numeric(0), stringsAsFactors=FALSE
    )),
    lapply(manifests, `[[`, "files")
  )))
}

#' The record that stands for every file in a store
#'
#' A record made before a file's hash was known, as of a download that failed,
#' is matched to the hash a later record found at the same location, so that the
#' two are taken for one file. Of the records of a file, the last that did not
#' fail stands for it, or the last of all where every run failed: only a failed
#' file is run again, so a file that failed after running is a second file of
#' the same bytes, whose rows are those of the run that did not.
#' @param records What `.storeRecords()` gives.
#' @return The records, one for each file, in the order they were written, with
#'   the hash found later in `file` where it was not known before.
#' @keywords internal
#' @noRd
.latestFiles <- function(records) {
  unknown <- is.na(records$file)
  records$file[unknown] <- .hashAt(records$location[unknown], records)
  key <- .fileKey(records$file, records$location, records$uri)
  #Within each file, failures before runs that did not fail, each in the order
  #written, so that the last row of each file is the one that stands for it.
  o <- order(key, records$status == "ok", seq_along(key), method="radix")
  chosen <- sort(o[!duplicated(key[o], fromLast=TRUE)])
  files <- records[chosen, , drop=FALSE]
  rownames(files) <- NULL
  return(files)
}

#' The hash a store found at each location
#'
#' A missing location is no location, so files that have none are not taken
#' for one another. Only a hash read from a file is used: a file that failed
#' before it was read, as one that did not match the hash the set gave it,
#' holds the hash the set gave.
#' @param location Locations.
#' @param records What `.storeRecords()` gives.
#' @return A hash for each location, `NA` where the store has none or the
#'   location is `NA`.
#' @keywords internal
#' @noRd
.hashAt <- function(location, records) {
  hashed <- records[!is.na(records$file) & !is.na(records$location) & records$observed, ,
                    drop=FALSE]
  hashed <- hashed[!duplicated(hashed$location, fromLast=TRUE), , drop=FALSE]
  return(hashed$file[match(location, hashed$location)])
}

#' Fill in the hashes of a set from a store
#' @param set The set.
#' @param records What `.storeRecords()` gives.
#' @return The set.
#' @keywords internal
#' @noRd
.withHashes <- function(set, records) {
  unknown <- which(is.na(set$hash))
  set$hash[unknown] <- .hashAt(set$location[unknown], records)
  return(set)
}

#' Report on a run of batches
#' @param ran The records of the files they ran.
#' @param what Which batches they were.
#' @return A message.
#' @keywords internal
#' @noRd
.batchReport <- function(ran, what) {
  return(paste0(
    what, ": ", nrow(ran), if (nrow(ran) == 1) " file, " else " files, ",
    sum(ran$status == "error"), " failed."
  ))
}

#' The id of a pipeline whose results are read
#' @param store The store.
#' @param p A pipeline, or an id.
#' @return The id, which has a folder in the store.
#' @keywords internal
#' @noRd
.resultsId <- function(store, p) {
  if (inherits(p, "Pipeline")) {
    id <- .pipelineId(p)
  } else if (is.character(p) && length(p) == 1 && !is.na(p) && nzchar(p)) {
    id <- p
  } else {
    stop("p must be a Pipeline, or the id of one.")
  }
  if (!file.exists(file.path(store, id, "pipeline.rds"))) {
    held <- list.dirs(store, full.names=FALSE, recursive=FALSE)
    held <- held[file.exists(file.path(store, held, "pipeline.rds"))]
    stop(
      "The store holds no results of the pipeline ", substr(id, 1, 8), ". ",
      if (length(held) == 0) "It holds none at all." else paste0(
        "It holds the results of ", paste(held, collapse=", "), ". An id ",
        "changes whenever the code of a package the pipeline uses does, and ",
        "any of these can be given as p."
      )
    )
  }
  return(id)
}

#' Read one table of results from every batch
#'
#' The rows of each file come from the batch it was last run in, and a file
#' whose last run failed has a row holding its error instead. Binding adds a
#' column the first rows lack at the end, as `window_time` is where only a later
#' file gives its start, so the keys are put back in front of `error` and the
#' values.
#' @param number The number of the table's step in the outline.
#' @param dir The folder of the pipeline in the store.
#' @param id The pipeline's id.
#' @param manifests What `.readManifests()` gives.
#' @param files What `.latestFiles()` gives.
#' @return A data frame.
#' @keywords internal
#' @noRd
.readTable <- function(number, dir, id, manifests, files) {
  parts <- lapply(manifests, function(m) {
    if (!number %in% m$tables$number) {
      return(NULL)
    }
    rows <- readRDS(.tablePath(dir, m$batch, number))
    last <- match(rows$file, files$file)
    here <- files$batch[last] == m$batch & files$status[last] == "ok"
    return(rows[!is.na(here) & here, , drop=FALSE])
  })
  failed <- files[files$status == "error", , drop=FALSE]
  if (nrow(failed) > 0) {
    parts[[length(parts) + 1]] <- data.frame(
      file=failed$file, pipeline=rep(id, nrow(failed)), error=failed$error,
      stringsAsFactors=FALSE
    )
  }
  out <- .bindResults(parts)
  values <- startsWith(names(out), "value")
  keys <- setdiff(names(out)[!values], "error")
  return(out[c(keys, "error", names(out)[values])])
}
