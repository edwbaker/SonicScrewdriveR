#' Create Default Cluster for Windowing
#'
#' Creates a default cluster using one less than the total cores available on the system. By default this uses forking, which may not be available on 'Windows'.
#' 
#' @param fork If TRUE uses forking to create the cluster
#' @keywords wave
#' @import parallel
#' @export
#' @examples
#' cl <- defaultCluster()
#' 
#' cl <- defaultCluster(FALSE)

defaultCluster <- function(fork=TRUE) {
  cores <- max(1, detectCores() - 1)
  if (fork) {
    cluster <- makeForkCluster(cores)
  } else {
    cluster <- makeCluster(cores)
  }
  return(cluster)
}