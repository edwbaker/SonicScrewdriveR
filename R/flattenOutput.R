#' Flatten the output of a process into rows
#'
#' The output of a process record can be anything a function returns: a single
#' number, a named vector of indices, a data frame of detected events, a list of
#' all of these, or nothing. `flattenOutput()` turns any of them into a data
#' frame, so that the results of a process applied to many recordings, or to
#' many windows of one, can be bound into one table.
#'
#' Every column name begins with `value`. A table of results needs columns of
#' its own beside these, to say which recording, which window and which process
#' a row came from, and those can be given any name that does not begin with
#' `value` without risk of clashing with a result.
#'
#' The rule is:
#'
#' \describe{
#'   \item{`NULL`}{No rows. There was nothing to flatten.}
#'   \item{A data frame}{Its rows, as they are: a detector that found five
#'     events gives five rows, and one that found none gives none. Each column
#'     is kept as it is, its name following `value.`, so a column `start` is
#'     `value.start`. Row names the data frame was given, rather than the
#'     numbers it has by default, are kept in a column of their own,
#'     `value.rownames`.}
#'   \item{Anything else}{One row, as below.}
#' }
#'
#' Within that one row:
#'
#' \describe{
#'   \item{A single value}{One column, `value`.}
#'   \item{A named vector}{One column for each element, its name following
#'     `value.`: `c(mean=440, sd=7.5)` gives `value.mean` and `value.sd`.}
#'   \item{A vector without names}{One column, `value`, holding the whole
#'     vector in a list cell.}
#'   \item{A named list}{Each element by these same rules, the names of an
#'     element and of what it holds joined by a full stop:
#'     `list(n=2, b=c(x=2, y=3))` gives `value.n`, `value.b.x` and `value.b.y`.
#'     An element that is `NULL` gives no column.}
#'   \item{A data frame within a list}{One column for each of its columns,
#'     holding the value where it has one row and the whole column in a list
#'     cell otherwise.}
#'   \item{Anything else}{One column, `value`, holding it in a list cell. This
#'     takes in a matrix, an S4 object, a list with a class of its own such as a
#'     test result, and a list or vector some of whose names are missing or
#'     repeated, whose elements could not each be given a column of their own.}
#' }
#'
#' A name the rule would give twice, as `list(a.b=1, a=list(b=2))` would, is
#' made unique with [make.unique()].
#'
#' Which columns there are depends on the names in the output and never on how
#' long its parts are, so that the rows from different recordings line up. What
#' a column holds can depend on length, though: an unnamed vector of one value
#' and one of five both give `value`, the first as a plain value and the second
#' in a list cell, and a data frame within a list gives plain values for one row
#' and list cells for more. A detector that finds one peak in one window and
#' three in the next therefore gives the same column of two types, and a table
#' binding such rows needs to hold that column as a list wherever any row has a
#' list cell in it. A column can also be missing from some rows, as where an
#' element is `NULL` in one output and not in another.
#'
#' The `processing` attribute [tagApply()] puts on a result that is not a wave
#' is dropped, as it is the history of the wave rather than part of the result.
#'
#' @param x The output of a process, as held in the `output` of a record in the
#'   `processing` slot of a `TaggedWave`, or any other result.
#' @return A data frame, with one row unless `x` is `NULL` or a data frame.
#' @seealso [tagApply()], which makes the records.
#' @export
#' @examples
#' flattenOutput(0.42)
#' flattenOutput(c(mean = 440, sd = 7.5))
#' flattenOutput(list(n = 2, peaks = c(440, 880)))
#' flattenOutput(data.frame(start = c(0.1, 0.6), end = c(0.3, 0.9)))
flattenOutput <- function(x) {
  #An environment is not copied when it is changed, so removing the attribute
  #from one would remove it from the caller's too.
  if (!is.environment(x) && !is.null(attr(x, "processing"))) {
    attr(x, "processing") <- NULL
  }
  if (is.null(x)) {
    return(.outputFrame(list(), 0L))
  }
  if (is.data.frame(x)) {
    cols <- as.list(x)
    #sprintf() rather than paste(), which gives "value." for a data frame
    #without columns rather than no name at all.
    names(cols) <- sprintf("value.%s", names(x))
    #Row names a data frame was given are part of what it says - the phase each
    #row of dayPhase() is - where the numbers it has by default, or those left
    #over from taking some of its rows, are not.
    if (is.character(attr(x, "row.names"))) {
      cols <- c(list(value.rownames=attr(x, "row.names")), cols)
    }
    return(.outputFrame(cols, nrow(x)))
  }
  return(.outputFrame(.flattenCells(x, "value"), 1L))
}

#' The columns of one row of a flattened output
#'
#' @param x The part of the output to flatten.
#' @param name The column name it is given, or that its own names are added to.
#' @return A named list holding a value of length one, or a list of length one
#'   that is a list cell, for each column.
#' @keywords internal
#' @noRd
.flattenCells <- function(x, name) {
  if (is.null(x)) {
    return(list())
  }
  if (is.data.frame(x)) {
    if (nrow(x) != 1) {
      return(.flattenNamed(lapply(x, function(col) list(col)), name, cells=TRUE))
    }
    #A list column holds its one value in a list, which would otherwise be read
    #as a list without names and kept whole.
    return(.flattenNamed(
      lapply(x, function(col) if (is.list(col) && !is.data.frame(col)) col[[1]] else col),
      name
    ))
  }
  if (is.atomic(x) && is.null(dim(x))) {
    if (.wellNamed(names(x))) {
      return(.flattenNamed(
        stats::setNames(lapply(seq_along(x), function(i) unname(x[i])), names(x)),
        name
      ))
    }
    if (length(x) == 1) {
      return(stats::setNames(list(unname(x)), name))
    }
  } else if (is.list(x) && !is.object(x) && .wellNamed(names(x))) {
    return(.flattenNamed(x, name))
  }
  return(stats::setNames(list(list(x)), name))
}

#' Flatten each element of a named list under its name
#' @param items A named list.
#' @param name The name the element names are added to.
#' @param cells Whether each element is already a cell, rather than something
#'   to flatten.
#' @return A named list of cells.
#' @keywords internal
#' @noRd
.flattenNamed <- function(items, name, cells=FALSE) {
  full <- sprintf("%s.%s", name, names(items))
  if (cells) {
    return(stats::setNames(items, full))
  }
  return(c(list(), unlist(
    lapply(seq_along(items), function(i) .flattenCells(items[[i]], full[i])),
    recursive=FALSE
  )))
}

#' Can every element be given a column under its own name?
#' @param nm The names of a vector or list.
#' @return A logical value.
#' @keywords internal
#' @noRd
.wellNamed <- function(nm) {
  length(nm) > 0 && !anyNA(nm) && all(nzchar(nm)) && !anyDuplicated(nm)
}

#' A data frame of flattened columns
#' @param cols A named list of columns, each `n` long.
#' @param n The number of rows.
#' @return A data frame.
#' @keywords internal
#' @noRd
.outputFrame <- function(cols, n) {
  return(structure(
    cols,
    names=make.unique(as.character(names(cols)), sep="."),
    class="data.frame",
    row.names=.set_row_names(n)
  ))
}
