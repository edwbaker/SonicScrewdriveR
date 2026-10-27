#' Create Default Cluster for Windowing
#'
#' Creates a default cluster using one less than the total cores available on
#' the system. Where the number of cores cannot be determined a cluster of one
#' is created. By default this uses forking, which is not be available on Windows.
#' Hence, the fork parameter has no effect on Windows.
#'
#' @param fork If TRUE uses forking to create the cluster (Unix like systems only)
#' @importFrom parallel detectCores makeForkCluster makeCluster
#' @export
#' @return A cluster object for parallel processing
#' @examples
#' \dontrun{
#' cl <- defaultCluster()
#' stopCluster(cl)
#' cl <- defaultCluster(FALSE)
#' stopCluster(cl)
#' }
#'
defaultCluster <- function(fork=TRUE) {
  #detectCores() returns NA where it cannot count the cores, and NA - 1L is NA,
  #so the lower bound of one has to be applied with na.rm=TRUE to catch it.
  cores <- max(1L, detectCores() - 1L, na.rm=TRUE)

  if (.Platform$OS.type == "windows") {
    fork <- FALSE
  }
  if (fork) {
    cluster <- makeForkCluster(cores, outfile="")
  } else {
    cluster <- makeCluster(cores, outfile="")
  }
  return(cluster)
}
