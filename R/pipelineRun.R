#' Run a pipeline on one wave
#'
#' What a run over a set of recordings does for each file once it is read.
#' @param p A `Pipeline`.
#' @param w A `Wave`, `WaveMC`, `TaggedWave` or `TaggedWaveMC` object. A wave
#'   without tags gives the same results, and keeps no history of how.
#' @return A list of `results`, a data frame for each analysis step that ran,
#'   named as the outline of the pipeline names it and in its order, and
#'   `wave`, the wave the pipeline carried to its end.
#' @keywords internal
#' @noRd
.runPipelineWave <- function(p, w) {
  if (!inherits(p, "Pipeline")) {
    stop("Can only run a Pipeline, not ", .aDescription(p), ".")
  }
  if (!inherits(w, c("Wave", "WaveMC"))) {
    stop("A pipeline runs on a Wave or WaveMC object, not ", .aDescription(w), ".")
  }
  outline <- .outline(p)
  names <- stats::setNames(outline$name, outline$number)
  return(.runSteps(p$steps, w, character(0), names))
}

#' Run steps in turn, each on the wave the one before it carried on
#' @param steps A list of steps.
#' @param w The wave the first of them gets.
#' @param number The number in the outline of the step they are the then= of.
#' @param names The name of each step's table of results, by its number.
#' @param keys Where in the recording the steps run, as a data frame of one row
#'   holding the columns of the cut and of each window they are inside, or
#'   `NULL` where they run on the whole recording.
#' @param offset How far into the recording, in seconds, the wave the first of
#'   them gets starts.
#' @return A list of `wave` and `results`, as `.runPipelineWave()` gives.
#' @keywords internal
#' @noRd
.runSteps <- function(steps, w, number, names, keys=NULL, offset=0) {
  results <- list()
  for (i in seq_along(steps)) {
    ran <- .runStep(steps[[i]], w, c(number, as.character(i)), names, keys, offset)
    w <- ran$wave
    #A cut without then= cuts the wave the steps after it get.
    keys <- ran$keys
    offset <- ran$offset
    results <- c(results, ran$results)
  }
  return(list(wave=w, results=results))
}

#' Run one step
#'
#' A wave given back goes on to the steps after this one, or to its then=,
#' while the steps after it get the wave as it was. Anything else is an
#' analysis: its result goes into its record, which goes into the history of
#' the wave carried on, and is turned into rows. A windowing given then= runs
#' those steps on each window instead. A cut moves where in the recording the
#' steps measuring what it cut run, which their rows say.
#' @param s A step.
#' @param w The wave it gets.
#' @param number Its number in the outline, as a character vector of its parts.
#' @param names The name of each step's table of results, by its number.
#' @param keys,offset Where in the recording it runs, as for `.runSteps()`.
#' @return A list of `wave`, `results`, and the `keys` and `offset` of the steps
#'   after it.
#' @keywords internal
#' @noRd
.runStep <- function(s, w, number, names, keys=NULL, offset=0) {
  if (!is.null(s$then) && .takesThen(s$fun)) {
    return(c(.runWindows(s, w, number, names, keys, offset), list(keys=keys, offset=offset)))
  }
  num <- paste(number, collapse=".")
  ran <- tryCatch(
    .callStep(s, w),
    error=function(e) .stepError(s, num, paste("failed:", conditionMessage(e)), e)
  )
  res <- ran$result
  if (inherits(res, c("Wave", "WaveMC"))) {
    out <- .keepRecord(w, res, ran$record)
    cut <- .cutStart(s, w)
    inside <- keys
    from <- offset
    if (!is.null(cut)) {
      from <- offset + cut
      inside <- .withCut(keys, from, from + length(res) / res@samp.rate)
    }
    if (is.null(s$then)) {
      return(list(wave=out, results=list(), keys=inside, offset=from))
    }
    return(list(
      wave=w, results=.runSteps(s$then$steps, out, number, names, inside, from)$results,
      keys=keys, offset=offset
    ))
  }
  if (.isWaveList(res)) {
    .stepError(s, num, paste0(
      "gave a list of ", length(res), if (length(res) == 1) " wave, " else " waves, ",
      if (is.null(s$then)) {
        "and a pipeline carries only one on."
      } else {
        "and only windowing() runs then= on several waves."
      }
    ))
  }
  if (!is.null(s$then)) {
    .stepError(s, num, paste0(
      "gave ", if (is.null(res)) "NULL" else .aDescription(res),
      " rather than a wave, so there is nothing to give its then=."
    ))
  }
  return(list(
    wave=.keepOutput(w, res, ran$record),
    results=stats::setNames(list(.withKeys(flattenOutput(res), keys)), names[[num]]),
    keys=keys, offset=offset
  ))
}

#' Where a cut begins in the wave it is made from
#'
#' cutws() and seewave's cutw() are the cuts a pipeline recognises, from the
#' step's own arguments, as it runs: the sample rate of the wave they cut is
#' known then, so a start in samples is placed in time exactly. A cut made by
#' any other function, one of the user's among them, is not recognised, and
#' what it gives is taken to start where the wave it was given did.
#' @param s The step.
#' @param w The wave it was given.
#' @return The start of the cut, in seconds from the start of `w`, or `NULL` if
#'   the step is not a cut.
#' @keywords internal
#' @noRd
.cutStart <- function(s, w) {
  if (identical(s$fun, cutws)) {
    from <- .describeCall(s$fun, s$args)$params$from
    return(if (is.null(from)) 0 else (from - 1) / w@samp.rate)
  }
  if (identical(s$fun, seewave::cutw)) {
    from <- .describeCall(s$fun, s$args)$params$from
    return(if (is.null(from)) 0 else from)
  }
  return(NULL)
}

#' The key columns of a cut
#'
#' A cut of a cut replaces the columns of the first, so that they give the span
#' that was measured.
#' @param keys The columns so far, or `NULL`.
#' @param start,end Where the cut starts and ends, in seconds from the start of
#'   the recording.
#' @return A data frame of one row.
#' @keywords internal
#' @noRd
.withCut <- function(keys, start, end) {
  if (is.null(keys)) {
    return(data.frame(cut_start=start, cut_end=end))
  }
  keys$cut_start <- start
  keys$cut_end <- end
  return(keys)
}

#' Does a step's function take its then= itself?
#'
#' windowing() runs the steps of its then= on each window, as its `FUN`, where
#' any other step gives its then= the wave it gives back.
#' @param fun The step's function.
#' @return A logical value.
#' @keywords internal
#' @noRd
.takesThen <- function(fun) {
  return(identical(fun, windowing))
}

#' Run the steps of a windowing's then= on each of its windows
#'
#' Each window is cut from the wave by windowing(), which records the cut in
#' the window's history, and its steps are run on it as the `FUN` of the
#' windowing. Their tables, one set from each window, are stacked, each row
#' keyed by the window it came from. The wave the steps after the windowing get
#' is the wave as it was, as for any step given then=.
#' @param s The windowing step.
#' @param w The wave it gets.
#' @param number Its number in the outline, as a character vector of its parts.
#' @param names The name of each step's table of results, by its number.
#' @param keys,offset Where in the recording it runs, as for `.runSteps()`.
#' @return A list of `wave` and `results`.
#' @keywords internal
#' @noRd
.runWindows <- function(s, w, number, names, keys, offset) {
  num <- paste(number, collapse=".")
  level <- sum(startsWith(as.character(names(keys)), "window_start")) + 1
  rate <- w@samp.rate
  #No `...`: windowing() hands FUN whatever it was given that is none of its
  #own arguments, which in a pipeline can only be a mistake.
  each <- function(section, start, len) {
    from <- offset + (start - 1) / rate
    here <- .windowKeys(level, from, length(section) / rate)
    inside <- if (is.null(keys)) here else cbind(keys, here)
    return(.runSteps(s$then$steps, section, number, names, inside, from)$results)
  }
  ran <- tryCatch(
    .callStep(s, w, list(FUN=each)),
    error=function(e) {
      #A step inside a window has already said which step it was.
      if (inherits(e, "pipelineStepError")) {
        stop(e)
      }
      .stepError(s, num, paste("failed:", conditionMessage(e)), e)
    }
  )
  windows <- ran$result
  if (length(windows) == 0) {
    seconds <- signif(length(w) / rate, 4)
    .stepError(s, num, paste0(
      "gave no windows, as the wave, of ", seconds,
      if (seconds == 1) " second" else " seconds", ", is shorter than one window."
    ))
  }
  tables <- unique(unlist(lapply(windows, names)))
  tables <- tables[order(match(tables, names))]
  results <- lapply(tables, function(t) .bindResults(lapply(windows, `[[`, t)))
  return(list(wave=w, results=stats::setNames(results, tables)))
}

#' The key columns of one window
#'
#' In seconds from the start of the recording. The outermost windowing's
#' columns are plain, and each windowing inside it adds a set numbered by how
#' deep it is, so a column means the same however deeply the windowing it
#' belongs to is nested.
#' @param level How many windowings deep the window is, 1 for the outermost.
#' @param start Where it starts.
#' @param length How long it is.
#' @return A data frame of one row.
#' @keywords internal
#' @noRd
.windowKeys <- function(level, start, length) {
  suffix <- if (level == 1) "" else paste0("_", level)
  return(stats::setNames(
    data.frame(start, start + length, length),
    paste0(c("window_start", "window_end", "window_length"), suffix)
  ))
}

#' Rows of results, with the window they came from in front
#' @param rows A data frame, as flattenOutput() gives.
#' @param keys The window, as `.windowKeys()` gives, or `NULL` outside any.
#' @return A data frame.
#' @keywords internal
#' @noRd
.withKeys <- function(rows, keys) {
  if (is.null(keys)) {
    return(rows)
  }
  n <- nrow(rows)
  return(structure(
    c(lapply(keys, rep, n), as.list(rows)),
    class="data.frame",
    row.names=.set_row_names(n)
  ))
}

#' Call the function of a step on a wave
#'
#' A function of this package may record itself, as it would when called
#' directly, and any other function is recorded as tagApply() records it. A
#' function of this package is one written at its top level, whose environment
#' is its namespace: a function made inside one of its functions, or written
#' where the namespace is only an ancestor, as in a test, makes no record of
#' its own to keep.
#' @param s A step.
#' @param w The wave.
#' @param extra Arguments to give the function besides the step's own, as the
#'   `FUN` a windowing's then= fills.
#' @return What `.tagApplyRecord()` gives.
#' @keywords internal
#' @noRd
.callStep <- function(s, w, extra=list()) {
  apply_one <- function(...) {
    .tagApplyRecord(
      w, s$fun, .funcName(s$fun, s$call[[1]]), NULL, NA, ...,
      .selfRecord=identical(environment(s$fun), asNamespace(.ownPackage()))
    )
  }
  return(do.call(apply_one, c(s$args, extra)))
}

#' Put the result of an analysis into its record, and the record into the
#' history of the wave
#'
#' The wave carried on is the wave that went in, so its digest out is its digest
#' in. The `processing` attribute tagApply() puts on a result is the history of
#' a wave rather than part of the result, and is left out, as it is when the
#' result is turned into rows.
#' @param w The wave the analysis was given.
#' @param res Its result.
#' @param record Its record, or `NULL` for none.
#' @return `w`, with the record added to its history.
#' @keywords internal
#' @noRd
.keepOutput <- function(w, res, record) {
  if (is.null(record)) {
    return(w)
  }
  if (!is.environment(res) && !is.null(attr(res, "processing"))) {
    attr(res, "processing") <- NULL
  }
  #Assigned as a list so that a NULL result is kept as the output, rather than
  #taking the field out of the record.
  record["output"] <- list(res)
  record$output_hash <- record$input_hash
  w@processing <- c(w@processing, list(record))
  return(w)
}

#' Is a result several waves?
#' @param x A result.
#' @return A logical value.
#' @keywords internal
#' @noRd
.isWaveList <- function(x) {
  return(
    is.list(x) && !is.object(x) && length(x) > 0 &&
      all(vapply(x, inherits, logical(1), what=c("Wave", "WaveMC")))
  )
}

#' Stop with an error naming the step it is in
#'
#' A run over thousands of files reports what failed and where, so the error
#' names the step by its number in the outline and its call, and has a class of
#' its own. An error from the step's function is kept as its `parent`.
#' @param s The step.
#' @param num Its number in the outline.
#' @param msg What went wrong.
#' @param parent The condition that caused it, or `NULL`.
#' @keywords internal
#' @noRd
.stepError <- function(s, num, msg, parent=NULL) {
  stop(structure(
    class=c("pipelineStepError", "error", "condition"),
    list(
      message=paste0("Step ", num, " ", .deparseOne(s$call), " ", msg),
      call=NULL, step=num, parent=parent
    )
  ))
}

#' Stack tables of results
#'
#' The rows of one step from many files, or many windows, in one table. A
#' column holds a value in each row where every table holds one, and a list
#' cell in each row where any table holds a list there, as [flattenOutput()]
#' gives a list cell for a result too long to be a value: one peak in one window
#' and three in the next give the same column of two types. A column some tables
#' lack is `NA` in their rows.
#' @param frames A list of data frames.
#' @return A data frame.
#' @keywords internal
#' @noRd
.bindResults <- function(frames) {
  frames <- Filter(Negate(is.null), frames)
  rows <- vapply(frames, nrow, integer(1))
  columns <- unique(unlist(lapply(frames, names)))
  out <- lapply(columns, function(col) {
    parts <- lapply(frames, function(f) if (col %in% names(f)) f[[col]])
    as_list <- any(vapply(parts, is.list, logical(1)))
    template <- Filter(Negate(is.null), parts)[[1]]
    filled <- lapply(seq_along(parts), function(i) {
      part <- parts[[i]]
      if (is.null(part)) {
        return(if (as_list) rep(list(NA), rows[i]) else template[rep(NA_integer_, rows[i])])
      }
      if (as_list && !is.list(part)) {
        return(as.list(part))
      }
      return(part)
    })
    if (as_list) {
      return(unlist(filled, recursive=FALSE))
    }
    return(do.call(c, unname(filled)))
  })
  return(structure(
    stats::setNames(out, columns),
    class="data.frame",
    row.names=.set_row_names(sum(rows))
  ))
}
