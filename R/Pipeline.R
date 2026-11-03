#' Write an analysis as a pipeline of steps
#'
#' A `Pipeline` is an analysis written down as steps, to be run on every
#' recording in a [RecordingSet][recordingSet()]. Each step is written as an
#' ordinary call to a function with the wave left out, and the wave is put in as
#' its first argument when the step runs, as [tagApply()] does.
#'
#' @section Writing a pipeline:
#' `pipeline()` keeps each call as it was written, without running it, so that
#' printing a pipeline shows what was typed. The arguments of each call are
#' evaluated once, when the pipeline is made: `bandpass(2000, 11000)` becomes
#' its `WaveFilter` then, so a pipeline is fixed and can be identified, and a
#' variable changed afterwards does not change it.
#'
#' A pipeline given as a step is run in its place, so `pipeline(p1, p2)` runs
#' the steps of both in turn, and is the same pipeline, with the same id, as one
#' written out with all of their steps. A pipeline is therefore a step, and a
#' step on its own is a pipeline of one: either can go wherever the other can.
#'
#' `task()` makes the same step as a call written in `pipeline()` would, as a
#' value, so that a pipeline can be built in code. A call written inside
#' `lapply()` would run there and then, where `task()` keeps it to run later:
#' `pipeline(lapply(c(1000, 4000), function(f) task(filterWave,
#' bandpass(f, 2 * f))))`. Its function is named `.fun` so that an argument
#' called `f`, as seewave calls a sampling frequency, reaches the function
#' rather than being taken for it, as with [tagApply()].
#'
#' An argument of `pipeline()` that is a call is a step, unless it is a call to
#' `pipeline()` or `task()`, or to one of the functions of base R that build a
#' list, such as `list()` and `lapply()`, which take no wave. A pipeline held in
#' a variable, or a list of them, is given by its name, and `NULL` gives no
#' steps.
#'
#' A pipeline does not carry the recordings it is run on. They are given when
#' it is run, so that the same pipeline can be run on any number of sets, and
#' its id is the same whichever set it runs on.
#'
#' @section How waves and results pass between steps:
#' What a step does to the pipeline depends on what it gives back, which is known
#' only once it has run:
#'
#' \describe{
#'   \item{A wave}{The steps after it get the new wave, so a chain of
#'     transformations stays flat: `pipeline(normalise(), dutyCycle())`
#'     measures the normalised wave.}
#'   \item{Several waves}{An error, as there is no single wave to carry on
#'     with.}
#'   \item{Anything else}{The step is an analysis, and the wave it was given
#'     goes on to the next step as it was.}
#' }
#'
#' `then=` gives a step a pipeline, or a single step, of its own. The wave the
#' step gives goes to that pipeline, and the steps after it get the wave as it
#' was before it, so `filterWave(bandpass(2000, 11000), then = dutyCycle())`
#' measures the band and leaves the rest of the pipeline the whole recording.
#' `then=` is taken out of the call before the function is called, so a function
#' with an argument of its own called `then` cannot be given it through a
#' pipeline. A step given `then=` that turns out not to give a wave is an error,
#' raised when it runs, and so for now is one that gives several.
#'
#' The results of a pipeline are a table for each analysis step, its result
#' turned into rows by [flattenOutput()], named by the step's path through the
#' pipeline: its own call after the calls of the steps whose `then=` it is in,
#' joined by a slash, as `filterWave(bandpass(2000, 11000)) / dutyCycle()`. Two
#' steps can have the same path, as the same analysis before and after a
#' transformation do, and each is then followed by its number in the outline,
#' as `dutyCycle() [3]`.
#'
#' A function of this package that records itself, as [filterWave()] and
#' [normalise()] do, leaves the record it makes when called directly in the
#' history of the wave, and any other step is recorded as [tagApply()] records
#' it. For an analysis, Pipeline puts the result in the `output` of its record,
#' sets the record's `output_hash` to its `input_hash`, as the wave carried on is
#' the wave that went in, and adds the record to the history of the wave, so
#' that every result traces back to what was done to the wave before it.
#'
#' @section Identity:
#' A pipeline is identified by a hash of its steps in order, so two pipelines
#' that would compute the same thing share an id, and changing anything that
#' could change a number gives a new one. A step's part of it covers its
#' function, its evaluated arguments, named as the function names them so that
#' one given by position counts the same as one given by name, the defaults it
#' was not given, and the code of every package it uses. A step with `then=`
#' adds the id of its own pipeline, which is the same whether that pipeline is
#' run on its own or inside another.
#'
#' Functions are identified by their code, never as R objects. A function object
#' changes when R byte-compiles it, and carries its environment and the source
#' it was read from, so it would give the same pipeline a new id in every
#' session. A function from a package is identified by its package and name,
#' with the code of that package. Any other function, written by the user or
#' inline, is identified by its own code, so a change to another function it
#' calls is not seen, and nor are values a function made by another function
#' holds from where it was made: give those as arguments instead.
#'
#' The packages a step uses are found from its code. From its function, and any
#' function among its arguments such as the one in a `WaveFilter`, calls are
#' followed through the functions of this package and of the user, and every
#' other package they reach is counted, though calls within it are not followed.
#' This package is always counted, as its own code runs around every step, and
#' scikit-maad is counted wherever a step reaches [getMaad()]. A package is
#' hashed from the deparsed code of every function in its namespace, and
#' scikit-maad from its installed source files, so any change to a package a
#' step uses gives it a new id, even a change to a function the step never
#' calls, and so does a version that was never released. Packages that come
#' with R are covered by R's version, which is part of every id. As the packages
#' are found from the code rather than from what ran, a package reached only on
#' a path a step does not take, such as one used to install another, still
#' counts, and one that is not installed is counted as not installed.
#'
#' Code is hashed each time an id is worked out rather than remembered, so a
#' package changed during a session is seen the next time one is. An argument
#' holding an environment, such as a Python module, cannot be identified, and
#' gives an error.
#'
#' @section Printing:
#' Printing a pipeline gives the start of its id, its steps as an outline
#' numbered so that a table of results can be found in it, each call as it was
#' written, and a last line of the code it was identified by: the start of the
#' hash of each package, and R's version.
#'
#' @param ... For `pipeline()`, the steps: calls written with the wave left out,
#'   pipelines, or lists of pipelines. For `task()`, the arguments of `.fun`
#'   other than the wave.
#' @param .fun A function, or the name of one, taking the wave as its first
#'   argument.
#' @param then Optional. A pipeline, or a single step, to give the wave the step
#'   gives back, as described in Details.
#' @return A `Pipeline`. `task()` gives a pipeline of one step.
#' @seealso [tagApply()], whose records the steps leave, and [flattenOutput()],
#'   which turns their results into rows.
#' @export
#' @examples
#' p <- pipeline(
#'   normalise(),
#'   filterWave(bandpass(2000, 11000), then = dutyCycle()),
#'   dutyCycle()
#' )
#' p
#'
#' # Two bands, built in code
#' pipeline(lapply(c(1000, 4000), function(f) {
#'   task(filterWave, bandpass(f, 2 * f), then = dutyCycle())
#' }))
pipeline <- function(...) {
  env <- parent.frame()
  exprs <- eval(substitute(alist(...)))
  #An argument left empty, as a comma too many leaves one, is the empty symbol.
  empty <- vapply(seq_along(exprs), function(i) identical(exprs[[i]], quote(expr=)), logical(1))
  if (any(empty)) {
    stop("Step ", which(empty)[1], " of pipeline() is empty. Is there a comma too many?")
  }
  return(.newPipeline(unlist(lapply(exprs, .stepsOf, env), recursive=FALSE)))
}

#' @rdname pipeline
#' @export
task <- function(.fun, ..., then = NULL) {
  env <- parent.frame()
  fun_expr <- substitute(.fun)
  fun <- .fun
  if (is.character(fun) && length(fun) == 1) {
    fun <- get0(fun, envir=env, mode="function")
  }
  if (!is.function(fun)) {
    stop("task() needs a function, or the name of one, not ", .deparseOne(fun_expr), ".")
  }
  values <- list(...)
  exprs <- eval(substitute(alist(...)))
  shown <- lapply(
    seq_along(values), function(i) .shownArgument(values[[i]], exprs[[i]], env)
  )
  names(shown) <- names(values)
  call <- as.call(c(list(.taskHead(fun, fun_expr, env)), shown))
  then_expr <- substitute(then)
  return(.newPipeline(list(.newStep(fun, call, values, .thenOf(then_expr, env)))))
}

#' Make a pipeline
#' @param steps A list of steps, as `.newStep()` makes them.
#' @return A `Pipeline`.
#' @keywords internal
#' @noRd
.newPipeline <- function(steps) {
  return(structure(list(steps=unname(as.list(steps))), class="Pipeline"))
}

#' Make a step
#'
#' A step is held as its function, the call it is shown as, the arguments it is
#' given besides the wave, already evaluated, and the pipeline its `then=` gives
#' its wave to.
#' @param fun A function.
#' @param call The call as written, without `then`.
#' @param args The evaluated arguments, named as they were given.
#' @param then A `Pipeline`, or `NULL`.
#' @return A step.
#' @keywords internal
#' @noRd
.newStep <- function(fun, call, args, then) {
  return(list(fun=fun, call=call, args=args, then=then))
}

#' The steps an argument of pipeline() stands for
#'
#' A call is a step unless it is a call to `pipeline()` or `task()`, one of the
#' operators that only reach a value, `p[[1]]` or `pkg::p`, a function written
#' inline, or one of the functions of base R that build a list, so that
#' `pipeline(lapply(lengths, function(n) task(windowing, n)))` is the steps
#' lapply() makes: none of those takes a wave, so a call to one is evaluated
#' instead. Anything else is evaluated, and has to give a pipeline, a list of
#' them or `NULL`.
#' @param expr The argument as written, or its value where it was given one, as
#'   `do.call()` gives.
#' @param env Where it was written.
#' @return A list of steps.
#' @keywords internal
#' @noRd
.stepsOf <- function(expr, env) {
  if (is.call(expr) && !.evaluatesToSteps(expr, env)) {
    return(list(.captureStep(expr, env)))
  }
  value <- eval(expr, env)
  if (is.null(value)) {
    return(list())
  }
  if (inherits(value, "Pipeline")) {
    return(value$steps)
  }
  if (is.list(value) && !is.object(value) &&
      all(vapply(value, inherits, logical(1), what="Pipeline"))) {
    return(unlist(lapply(value, `[[`, "steps"), recursive=FALSE))
  }
  if (is.function(value)) {
    stop(
      "`", .deparseOne(expr), "` is a function. A step is a call to one with ",
      "the wave left out, as `", .deparseOne(expr), "()`, or task().",
      call.=FALSE
    )
  }
  if (inherits(value, "RecordingSet")) {
    stop(
      "`", .deparseOne(expr), "` is a RecordingSet. A pipeline does not carry ",
      "the recordings it is run on: they are given when it is run.",
      call.=FALSE
    )
  }
  stop(
    "A step is a call written with the wave left out, or a pipeline, and `",
    .deparseOne(expr), "` is ", .aDescription(value), ".",
    call.=FALSE
  )
}

#' Is a call written in pipeline() one to evaluate rather than a step?
#' @param expr A call.
#' @param env Where it was written.
#' @return A logical value.
#' @keywords internal
#' @noRd
.evaluatesToSteps <- function(expr, env) {
  if (.callTo(expr, c("[[", "[", "$", "@", "(", "{", "if", "::", ":::", "function"))) {
    return(TRUE)
  }
  fun <- .stepFunction(expr[[1]], env)
  return(any(vapply(
    c(list(pipeline, task), .listBuilders), identical, logical(1), fun
  )))
}

#' The functions of base R that build a list, which pipeline() evaluates
#' @keywords internal
#' @noRd
.listBuilders <- list(
  base::list, base::c, base::lapply, base::Map, base::mapply, base::do.call,
  base::Reduce, base::Filter, base::append, base::rev
)

#' Is a call one to an operator or function named as written?
#' @param expr A call.
#' @param names The names it might be called by.
#' @return A logical value.
#' @keywords internal
#' @noRd
.callTo <- function(expr, names) {
  return(is.name(expr[[1]]) && as.character(expr[[1]]) %in% names)
}

#' The function a step calls
#'
#' A name is looked up as R looks up the function of a call, passing over
#' anything of that name that is not a function, so a step `filter()` is a call
#' to the function even where a variable called `filter` holds data.
#' @param head The function part of the call.
#' @param env Where the call was written.
#' @return A function.
#' @keywords internal
#' @noRd
.stepFunction <- function(head, env) {
  if (is.name(head) || is.character(head)) {
    fun <- get0(as.character(head), envir=env, mode="function")
  } else {
    fun <- tryCatch(eval(head, env), error=function(e) NULL)
  }
  if (!is.function(fun)) {
    stop("Could not find the function of the step ", .deparseOne(head), "().",
         call.=FALSE)
  }
  return(fun)
}

#' Capture a call written in pipeline() as a step
#'
#' The arguments are evaluated now, where the call was written, and `then=` is
#' taken out to be the step's own pipeline.
#' @param call The call.
#' @param env Where it was written.
#' @return A step.
#' @keywords internal
#' @noRd
.captureStep <- function(call, env) {
  fun <- .stepFunction(call[[1]], env)
  args <- as.list(call)[-1]
  given <- names(args)
  if (is.null(given)) {
    given <- rep("", length(args))
  }
  is_then <- given == "then"
  if (sum(is_then) > 1) {
    stop("then= is given more than once in ", .deparseOne(call), ".", call.=FALSE)
  }
  written <- as.call(c(list(call[[1]]), args[!is_then]))
  #base::list itself rather than its name, which a function of the user's could
  #take where the call is evaluated.
  values <- eval(as.call(c(list(base::list), args[!is_then])), env)
  then <- if (any(is_then)) .thenOf(args[[which(is_then)]], env) else NULL
  return(.newStep(fun, written, values, then))
}

#' The pipeline a then= stands for
#'
#' Whatever `pipeline()` takes as a step: a call written there, a pipeline or a
#' list of them. `NULL` is no `then=` at all.
#' @param expr The `then=` argument as written.
#' @param env Where it was written.
#' @return A `Pipeline`, or `NULL`.
#' @keywords internal
#' @noRd
.thenOf <- function(expr, env) {
  if (is.null(expr)) {
    return(NULL)
  }
  return(.newPipeline(.stepsOf(expr, env)))
}

#' How the function of a step made by task() is shown
#'
#' A function given by name is shown by that name where the name means the same
#' thing anywhere, as a name of the global environment or of a package does, so
#' `task(normalise)` shows as `normalise()`. A name that is a variable of a
#' function, as `f` is inside `lapply(fs, function(f) task(f))`, would mean
#' nothing outside it, so the function is shown as it would be called from
#' outside its package instead, and a function of no package as its code.
#' @param fun The function.
#' @param expr The argument `fun` came from.
#' @param env Where it was written.
#' @return A name, a call or the function.
#' @keywords internal
#' @noRd
.taskHead <- function(fun, expr, env) {
  if (is.character(expr) && length(expr) == 1) {
    expr <- as.name(expr)
  }
  if (is.call(expr) && .callTo(expr, c("::", ":::", "function"))) {
    return(expr)
  }
  if (is.name(expr) && is.null(.localFrame(as.character(expr), env))) {
    return(expr)
  }
  name <- .functionName(fun)
  if (identical(name, "<anonymous function>")) {
    return(fun)
  }
  return(str2lang(name))
}

#' The frame of a function a name is a variable of
#'
#' Looked for from where it was written up to the global environment or the
#' package it was written in, whose names mean the same thing anywhere.
#' @param name A name.
#' @param env Where it was written.
#' @return The environment, or `NULL` where the name is not a variable of a
#'   function.
#' @keywords internal
#' @noRd
.localFrame <- function(name, env) {
  frame <- env
  while (!identical(frame, topenv(frame)) && !identical(frame, emptyenv())) {
    if (exists(name, envir=frame, inherits=FALSE)) {
      return(frame)
    }
    frame <- parent.env(frame)
  }
  return(NULL)
}

#' How an argument of a step made by task() is shown
#'
#' A short value is shown as itself, so the `n` of `lapply(c(1, 60),
#' function(n) task(windowing, n))` shows as the number it was. Anything else is
#' shown as it was written, `bandpass(2000, 11000)` for a filter, with any
#' variable of the function that wrote it shown as its value where that is
#' short, as such a variable means nothing outside it: two filters made by
#' `lapply(c(1000, 4000), function(f) task(filterWave, bandpass(f, 2 * f)))`
#' would otherwise both show as `bandpass(f, 2 * f)`. Something given as a
#' value, with nothing written, is shown by its class.
#' @param value The argument.
#' @param expr The argument as written.
#' @param env Where it was written.
#' @return Something to put in a call.
#' @keywords internal
#' @noRd
.shownArgument <- function(value, expr, env) {
  if (.isShortValue(value)) {
    return(value)
  }
  if (is.name(expr) || is.call(expr)) {
    return(.withLocalValues(expr, env))
  }
  return(as.name(paste0("<", class(value)[1], ">")))
}

#' Is a value short enough to show as itself?
#' @param x A value.
#' @return A logical value.
#' @keywords internal
#' @noRd
.isShortValue <- function(x) {
  return(is.null(x) || (
    is.atomic(x) && is.null(attributes(x)) && length(x) <= 10 &&
      nchar(.deparseOne(x)) <= 60
  ))
}

#' An expression with the short variables of the function it was written in
#' shown as their values
#'
#' Only a variable of a function, and not one of the global environment or of a
#' package, which mean the same thing anywhere, and never the function of a
#' call.
#' @param expr An expression.
#' @param env Where it was written.
#' @return The expression.
#' @keywords internal
#' @noRd
.withLocalValues <- function(expr, env) {
  if (is.name(expr)) {
    frame <- .localFrame(as.character(expr), env)
    if (is.null(frame)) {
      return(expr)
    }
    value <- get(as.character(expr), envir=frame)
    return(if (.isShortValue(value)) value else expr)
  }
  if (is.call(expr)) {
    #What follows $ or @ is the name of a part, not a variable.
    parts <- if (.callTo(expr, c("$", "@"))) 2 else seq_along(expr)[-1]
    for (i in parts) {
      #An argument left empty, as in x[, 1], is the empty symbol, which cannot be
      #held in a variable.
      if (is.name(expr[[i]]) && !nzchar(as.character(expr[[i]]))) {
        next
      }
      #Assigned as a list, so that a NULL is put in rather than taking the
      #argument out.
      expr[i] <- list(.withLocalValues(expr[[i]], env))
    }
  }
  return(expr)
}

#' Deparse an expression onto one line
#'
#' Without backticks, so that an argument shown by its class reads as
#' `<WaveFilter>`.
#' @param x An expression or value.
#' @return A single string.
#' @keywords internal
#' @noRd
.deparseOne <- function(x) {
  return(paste(trimws(deparse(x, width.cutoff=500L, backtick=FALSE)), collapse=" "))
}

#' What sort of thing a value is, for a message
#' @param x A value.
#' @return A string, as "a RecordingSet".
#' @keywords internal
#' @noRd
.aDescription <- function(x) {
  cls <- class(x)[1]
  return(paste(if (grepl("^[AEIOUaeiou]", cls)) "an" else "a", cls))
}

#' The outline of a pipeline
#'
#' Every step, in the order a run reaches them, with its number in the outline,
#' its call, its path through the pipeline and the name its table of results
#' has. A path is the same wherever two steps make the same calls, so a name is
#' made unique by adding the step's number to every path that is not.
#' @param p A `Pipeline`.
#' @return A data frame with one row per step.
#' @keywords internal
#' @noRd
.outline <- function(p) {
  rows <- .outlineRows(p$steps, character(0), character(0), 0)
  out <- data.frame(
    number=vapply(rows, `[[`, character(1), "number"),
    label=vapply(rows, `[[`, character(1), "label"),
    indent=vapply(rows, `[[`, numeric(1), "indent"),
    text=vapply(rows, `[[`, character(1), "text"),
    then=vapply(rows, `[[`, logical(1), "then"),
    path=vapply(rows, `[[`, character(1), "path"),
    stringsAsFactors=FALSE
  )
  repeated <- out$path %in% out$path[duplicated(out$path)]
  out$name <- ifelse(repeated, paste0(out$path, " [", out$number, "]"), out$path)
  return(out)
}

#' The rows of an outline for some steps, and those of their then= in turn
#' @param steps A list of steps.
#' @param number The number of the step they are the `then=` of.
#' @param path The calls of the steps they are within.
#' @param indent How far they are indented.
#' @return A list of rows.
#' @keywords internal
#' @noRd
.outlineRows <- function(steps, number, path, indent) {
  rows <- list()
  for (i in seq_along(steps)) {
    s <- steps[[i]]
    num <- c(number, as.character(i))
    label <- if (length(num) == 1) paste0(num, ".") else paste(num, collapse=".")
    text <- .deparseOne(s$call)
    rows[[length(rows) + 1]] <- list(
      number=paste(num, collapse="."), label=label, indent=indent, text=text,
      then=!is.null(s$then), path=paste(c(path, text), collapse=" / ")
    )
    if (!is.null(s$then)) {
      #Each level lines up under the text of the step it is within.
      rows <- c(rows, .outlineRows(
        s$then$steps, num, c(path, text), indent + nchar(label) + 1
      ))
    }
  }
  return(rows)
}

#' @export
format.Pipeline <- function(x, ...) {
  outline <- .outline(x)
  n <- nrow(outline)
  identity <- tryCatch(.pipelineIdentity(x), error=function(e) e)
  steps <- paste0(n, if (n == 1) " step" else " steps")
  if (inherits(identity, "error")) {
    header <- paste0("<Pipeline: ", steps, ">")
    code <- paste0("Code: not hashed, as ", conditionMessage(identity))
  } else {
    header <- paste0("<Pipeline ", substr(identity$id, 1, 8), ": ", steps, ">")
    #A pipeline with no steps runs no code.
    code <- paste0("Code: ", if (length(identity$code) == 0) "none" else paste(
      names(identity$code), .shortCode(identity$code), collapse=", "
    ))
  }
  lines <- if (n > 0) paste0(
    strrep(" ", outline$indent), outline$label, " ", outline$text,
    ifelse(outline$then, ", then", "")
  )
  return(c(header, lines, code))
}

#' @export
print.Pipeline <- function(x, ...) {
  cat(format(x, ...), sep="\n")
  return(invisible(x))
}

#' The start of each code hash, for printing
#'
#' A hash is shown by its first eight characters, which is enough to tell two
#' apart by eye. R is shown by its version, which is what it is hashed as.
#' @param code The named code hashes of `.pipelineIdentity()`.
#' @return A character vector.
#' @keywords internal
#' @noRd
.shortCode <- function(code) {
  return(ifelse(
    names(code) == "R" | !grepl("^[0-9a-f]{64}$", code), code, substr(code, 1, 8)
  ))
}
