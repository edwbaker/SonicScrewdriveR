#' Convert angle to degrees
#'
#' Converts angle measurements into degrees
#'
#' @param A The angle value to convert
#' @param input.unit The unit of angle to convert, one of "degrees", "radians".
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return The numeric value in degrees
#'
convert2degrees <- function(A, input.unit="radians", input) {
  redone <- .withRenamedArgs(
    sys.call(), parent.frame(), c(input="input.unit"), "convert2degrees"
  )
  if (!is.null(redone)) {
    return(redone[[1]])
  }
  .convertLinear(A, input.unit, "degrees", .angleMultipliers(),
                 what="input.unit", fn="convert2degrees")
}

#' Convert angle to radians
#'
#' Converts angle measurements into radians
#'
#' @param A The angle value to convert
#' @param input.unit The unit of angle to convert, one of "degrees", "radians".
#' @param input Deprecated, use input.unit instead.
#' @export
#' @return The numeric value in radians
#'
convert2radians <- function(A, input.unit="degrees", input) {
  redone <- .withRenamedArgs(
    sys.call(), parent.frame(), c(input="input.unit"), "convert2radians"
  )
  if (!is.null(redone)) {
    return(redone[[1]])
  }
  .convertLinear(A, input.unit, "radians", .angleMultipliers(),
                 what="input.unit", fn="convert2radians")
}
