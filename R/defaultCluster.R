#' Create Default Cluster for Windowing
#'
#' Creates a default cluster using one less than the total cores available on
#' the system. Where the number of cores cannot be determined a cluster of one
#' is created. By default this uses forking, which is not be available on Windows.
#' Hence, the fork parameter has no effect on Windows.
#'
#' @details
#' The workers are given parallel random number streams, so that a windowed
#' analysis that uses randomness can be run again and give the same numbers.
#' Without them each worker seeds itself from its own entropy and `set.seed()`
#' in the session cannot reach it, so the analysis gives different numbers on
#' every run. Where `seed` is NULL the streams are drawn from the session's own
#' random number state, so `set.seed()` before the call fixes them, and that
#' state and the session's generator are left as they were found.
#'
#' The streams are one per worker, so which worker computes a window decides
#' which numbers it draws: reproducing a result needs a cluster of the same
#' size as well as the same seed, and the same result cannot be expected of
#' `windowing()` without a cluster, which draws from the session's own stream.
#'
#' @param fork If TRUE uses forking to create the cluster (Unix like systems only)
#' @param seed An integer to seed the workers' random number streams with, or
#'   NULL (default) to take them from the session's own random number state.
#' @importFrom parallel detectCores makeForkCluster makeCluster clusterSetRNGStream
#' @export
#' @return A cluster object for parallel processing
#' @examples
#' \dontrun{
#' cl <- defaultCluster()
#' stopCluster(cl)
#' cl <- defaultCluster(FALSE)
#' stopCluster(cl)
#'
#' # The same numbers on every run, from the session's seed or from the
#' # cluster's own.
#' set.seed(42)
#' cl <- defaultCluster()
#' stopCluster(cl)
#' cl <- defaultCluster(seed=42)
#' stopCluster(cl)
#' }
#'
defaultCluster <- function(fork=TRUE, seed=NULL) {
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
  #A worker that is given no stream seeds itself from its own entropy, so a
  #windowed analysis using randomness gave different numbers on every run and
  #set.seed() in the session made no difference to them. This hands each worker
  #a stream of "L'Ecuyer-CMRG", which is the generator that has them, taken
  #from the session's own state where no seed is given, and puts that state and
  #the session's generator back as it found them.
  clusterSetRNGStream(cluster, iseed=seed)
  return(cluster)
}
