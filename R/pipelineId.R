#' The id of a pipeline, and the code it was worked out from
#'
#' The code of each package is hashed once here, however many steps use it, and
#' not kept beyond the call: a run works its id out once, at its start, and a
#' hash kept for a whole session would go on describing a package after it was
#' reloaded or updated.
#' @param p A `Pipeline`.
#' @return A list of `id`, a SHA-256 hex string, and `code`, the hash of each
#'   package's code and R's version, named by package, this package first, then
#'   the other R packages, then Python modules, then R.
#' @keywords internal
#' @noRd
.pipelineIdentity <- function(p) {
  code <- new.env(parent=emptyenv())
  id <- .pipelineHash(p, code)
  found <- ls(code, all.names=TRUE, sorted=FALSE)
  python <- names(.pythonModules)
  order <- c(
    .ownPackage(),
    sort(setdiff(found, c(.ownPackage(), python, "R")), method="radix"),
    intersect(python, found),
    "R"
  )
  order <- order[order %in% found]
  return(list(id=id, code=unlist(mget(order, envir=code))))
}

#' The id of a pipeline
#' @param p A `Pipeline`.
#' @return A SHA-256 hex string.
#' @keywords internal
#' @noRd
.pipelineId <- function(p) {
  return(.pipelineIdentity(p)$id)
}

#' Hash a pipeline's steps in order
#' @param p A `Pipeline`.
#' @param code An environment holding the code hashes worked out so far.
#' @return A SHA-256 hex string.
#' @keywords internal
#' @noRd
.pipelineHash <- function(p, code) {
  return(.idDigest(list(
    "Pipeline", vapply(p$steps, .stepHash, character(1), code)
  )))
}

#' Hash one step
#'
#' Its function, its arguments named as the function names them, the defaults it
#' was not given, written out whole, the code of the packages it uses, and the
#' id of the pipeline its then= gives its wave to.
#' @param s A step.
#' @param code An environment holding the code hashes worked out so far.
#' @return A SHA-256 hex string.
#' @keywords internal
#' @noRd
.stepHash <- function(s, code) {
  call <- .describeCall(s$fun, s$args, deparser=.wholeDeparse)
  packages <- .stepPackages(s)
  return(.idDigest(list(
    fun=.functionIdentity(s$fun),
    params=.idValue(call$params),
    defaults=call$defaults,
    code=vapply(packages, .codeHash, character(1), code),
    then=if (!is.null(s$then)) .pipelineHash(s$then, code)
  )))
}

#' Deparse an expression whole, onto one line
#' @param x An expression.
#' @return A single string.
#' @keywords internal
#' @noRd
.wholeDeparse <- function(x) {
  return(paste(deparse(x, width.cutoff=500L), collapse="\n"))
}

#' Digest something for an id
#' @param x A list of plain values, as `.idValue()` gives.
#' @return A SHA-256 hex string.
#' @keywords internal
#' @noRd
.idDigest <- function(x) {
  return(digest::digest(x, algo="sha256"))
}

#' What identifies a function
#'
#' Its package and name, as `pkg::name`, where it is bound to a name in a
#' package, as its package's code is hashed beside it. Otherwise its code: the
#' function object itself is not hashed, as it changes when R byte-compiles it
#' and carries its environment and the source it was read from.
#' @param fun A function.
#' @return A single string.
#' @keywords internal
#' @noRd
.functionIdentity <- function(fun) {
  name <- .functionName(fun)
  if (grepl("::", name, fixed=TRUE)) {
    return(name)
  }
  return(.functionText(fun))
}

#' The code of a function, as text
#'
#' Deparsed without its attributes, so neither the source it was read from nor a
#' class it was given comes into it, and from the function within an S4 method
#' or generic rather than the object wrapping it.
#' @param fun A function.
#' @return A single string.
#' @keywords internal
#' @noRd
.functionText <- function(fun) {
  if (isS4(fun) && methods::.hasSlot(fun, ".Data")) {
    fun <- fun@.Data
  }
  return(paste(
    deparse(fun, width.cutoff=500L, control=c("keepNA", "keepInteger", "niceNames")),
    collapse="\n"
  ))
}

#' An argument of a step, in a form that can be hashed
#'
#' Plain values are hashed as they are. A function is replaced by what
#' identifies it, and the structure of a list, an S4 object or a value with
#' attributes is kept around the forms of what it holds, so that the function a
#' `WaveFilter` holds is identified the same way as any other. An environment
#' cannot be identified: what it holds can change after the pipeline is made,
#' and a Python module is one.
#' @param x A value.
#' @return A value holding no function or environment.
#' @keywords internal
#' @noRd
.idValue <- function(x) {
  if (is.function(x)) {
    return(list(.function=.functionIdentity(x)))
  }
  if (is.environment(x) || typeof(x) %in% c("externalptr", "weakref")) {
    stop(
      "A pipeline cannot be identified by an argument holding ",
      .aDescription(x), ", as what it holds can change. Leave it out, or ",
      "give what it holds instead.", call.=FALSE
    )
  }
  if (is.language(x)) {
    return(list(.code=.wholeDeparse(x)))
  }
  if (isS4(x)) {
    slots <- methods::slotNames(x)
    return(list(.class=class(x), .slots=stats::setNames(
      lapply(slots, function(s) .idValue(methods::slot(x, s))), slots
    )))
  }
  attrs <- attributes(x)
  if (length(attrs) > 0) {
    attrs <- attrs[setdiff(sort(names(attrs), method="radix"), "names")]
  }
  if (is.list(x)) {
    value <- lapply(seq_along(x), function(i) .idValue(.subset2(x, i)))
    names(value) <- names(unclass(x))
  } else {
    value <- x
    attributes(value) <- if (!is.null(names(x))) list(names=names(x))
  }
  if (length(attrs) == 0) {
    return(value)
  }
  return(list(.value=value, .attributes=lapply(attrs, .idValue)))
}

#' Python modules a step can use, by the function of this package that imports
#' each
#' @keywords internal
#' @noRd
.pythonModules <- list("scikit-maad"="getMaad")

#' Packages that come with R
#'
#' Their code changes only with R, whose version is part of every id, so they
#' are not hashed on their own.
#' @keywords internal
#' @noRd
.rPackages <- c(
  "base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
  "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils"
)

#' The packages a step uses
#'
#' Found from its code, as `.packagesUsed()` finds them, starting from its
#' function and any function among its arguments, with this package and R
#' always among them. The steps of its then= are not, as their pipeline's id
#' covers them.
#' @param s A step.
#' @return Package names, this package first and `R` last.
#' @keywords internal
#' @noRd
.stepPackages <- function(s) {
  used <- .packagesUsed(c(list(s$fun), .functionsIn(s$args)))
  return(c(
    .ownPackage(), sort(setdiff(used, c(.ownPackage(), "R")), method="radix"), "R"
  ))
}

#' The functions a value holds
#' @param x A value.
#' @return A list of functions, found in lists and in the slots of S4 objects.
#' @keywords internal
#' @noRd
.functionsIn <- function(x) {
  if (is.function(x)) {
    return(list(x))
  }
  if (isS4(x)) {
    return(unlist(
      lapply(methods::slotNames(x), function(s) .functionsIn(methods::slot(x, s))),
      recursive=FALSE
    ))
  }
  if (is.list(x)) {
    return(unlist(
      lapply(seq_along(x), function(i) .functionsIn(.subset2(x, i))),
      recursive=FALSE
    ))
  }
  return(list())
}

#' The packages some functions use, found from their code
#'
#' Every function a function's code names is looked up where the function
#' would look it up. Functions of this package, and those of no package, as
#' the user's are, are followed in turn, and a function of any other package
#' counts its package without being followed: following calls within packages
#' reaches dozens of packages a step never uses, through the ones they only
#' suggest. A package named as `pkg::name` counts, and so does a Python module
#' wherever the function of this package that imports it is reached. Packages
#' that come with R do not count, as R's version stands for them.
#' @param funs A list of functions.
#' @return Package and module names.
#' @keywords internal
#' @noRd
.packagesUsed <- function(funs) {
  own <- .ownPackage()
  gateways <- lapply(.pythonModules, get, envir=asNamespace(own))
  seen <- character(0)
  used <- character(0)
  queue <- funs
  while (length(queue) > 0) {
    f <- queue[[1]]
    queue <- queue[-1]
    if (is.primitive(f)) {
      next
    }
    #Two closures written alike can look their names up in different places.
    key <- paste(format(environment(f)), .functionText(f))
    if (key %in% seen) {
      next
    }
    seen <- c(seen, key)
    pkg <- .funPackage(f)
    if (!is.null(pkg) && !identical(pkg, own)) {
      used <- c(used, pkg)
      next
    }
    used <- c(used, names(gateways)[vapply(gateways, identical, logical(1), f)])
    code <- .functionCode(f)
    named <- .namespacedNames(code)
    used <- c(used, named$package)
    for (name in named$name[named$package == own]) {
      queue <- c(queue, list(get0(name, envir=asNamespace(own), mode="function")))
    }
    for (name in unique(unlist(lapply(code, all.names)))) {
      queue <- c(queue, list(get0(name, envir=environment(f), mode="function")))
    }
    queue <- Filter(Negate(is.null), queue)
  }
  return(setdiff(unique(used), .rPackages))
}

#' The code of a function: its body, and the defaults of its formals that are
#' calls
#' @param f A function.
#' @return A list of expressions.
#' @keywords internal
#' @noRd
.functionCode <- function(f) {
  formal_args <- formals(f)
  defaults <- list()
  for (i in seq_along(formal_args)) {
    #An argument with no default is the empty symbol, which cannot be held in a
    #variable without R taking it for a missing argument.
    if (is.call(formal_args[[i]])) {
      defaults[[length(defaults) + 1]] <- formal_args[[i]]
    }
  }
  return(c(list(body(f)), defaults))
}

#' The `pkg::name` and `pkg:::name` references in some code
#' @param code A list of expressions.
#' @return A data frame of `package` and `name`.
#' @keywords internal
#' @noRd
.namespacedNames <- function(code) {
  found <- character(0)
  walk <- function(e) {
    if (!is.call(e)) {
      return()
    }
    if (is.name(e[[1]]) && as.character(e[[1]]) %in% c("::", ":::")) {
      found <<- c(found, as.character(e[[2]]), as.character(e[[3]]))
      return()
    }
    for (i in seq_along(e)) {
      if (is.call(e[[i]])) {
        walk(e[[i]])
      }
    }
  }
  for (e in code) {
    walk(e)
  }
  pairs <- matrix(found, ncol=2, byrow=TRUE)
  return(data.frame(package=pairs[, 1], name=pairs[, 2], stringsAsFactors=FALSE))
}

#' The hash of the code of a package, of a Python module, or R's version
#' @param package A package name, a name in `.pythonModules`, or `"R"`.
#' @param code An environment holding the hashes worked out so far, which this
#'   one is added to.
#' @return A single string.
#' @keywords internal
#' @noRd
.codeHash <- function(package, code) {
  if (exists(package, envir=code, inherits=FALSE)) {
    return(get(package, envir=code))
  }
  if (identical(package, "R")) {
    value <- sub("^R version ", "", R.version.string)
  } else if (package %in% names(.pythonModules)) {
    value <- .pythonCodeHash(package)
  } else {
    value <- .packageCodeHash(package)
  }
  assign(package, value, envir=code)
  return(value)
}

#' The hash of the code of an R package
#'
#' Every function in its namespace, its internal functions as well as those it
#' exports, and every S4 method it defines, by name and in an order that does
#' not depend on the locale. The functions are deparsed rather than hashed as
#' objects, so the hash is the same whether or not they were byte-compiled or
#' kept their source.
#' @param package A package name.
#' @return A SHA-256 hex string, or "not installed".
#' @keywords internal
#' @noRd
.packageCodeHash <- function(package) {
  if (!requireNamespace(package, quietly=TRUE)) {
    return("not installed")
  }
  ns <- asNamespace(package)
  texts <- character(0)
  for (name in sort(ls(ns, all.names=TRUE), method="radix")) {
    obj <- get(name, envir=ns)
    if (is.function(obj)) {
      texts <- c(texts, name, .functionText(obj))
    } else if (startsWith(name, ".__T__") && is.environment(obj)) {
      #The methods for one generic, by signature.
      for (signature in sort(ls(obj, all.names=TRUE), method="radix")) {
        texts <- c(texts, paste(name, signature), .functionText(get(signature, envir=obj)))
      }
    }
  }
  return(digest::digest(paste(texts, collapse="\n"), algo="sha256", serialize=FALSE))
}

#' The hash of the installed source of a Python module
#' @param module A name in `.pythonModules`.
#' @return A SHA-256 hex string.
#' @keywords internal
#' @noRd
.pythonCodeHash <- function(module) {
  dir <- .maadSourceDir()
  files <- sort(list.files(dir, pattern="[.]py$", recursive=TRUE), method="radix")
  sums <- vapply(
    file.path(dir, files), digest::digest, character(1), algo="sha256", file=TRUE
  )
  return(digest::digest(
    paste(files, sums, collapse="\n"), algo="sha256", serialize=FALSE
  ))
}

#' Where scikit-maad is installed
#'
#' Found by importing it as a step does, but without installing it where it is
#' missing: working out an id should not install anything.
#' @return The folder holding scikit-maad's source.
#' @keywords internal
#' @noRd
.maadSourceDir <- function() {
  if (!package.installed("reticulate", askInstall=FALSE) ||
      !reticulate::virtualenv_exists(envname="sonicscrewdriver")) {
    stop(
      "scikit-maad is not installed, so the code a step runs in it cannot be ",
      "hashed. pythonInstall() installs it.", call.=FALSE
    )
  }
  return(dirname(getMaad()$`__file__`))
}
