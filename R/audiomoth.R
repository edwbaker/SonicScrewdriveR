#' Read audiomoth configuration file
#'
#' Reads and parses an audiomoth configuration file.
#'
#' @param filename Path to the config file to read
#' @return A data frame of matching annotations
#' @export
#' @examples
#' \dontrun{
#' audiomoth_config("./CONFIG.TXT")
#' }
#'
audiomoth_config <- function(filename) {
  f <- readLines(filename)
  c <- read.csv(textConnection(sub(":", "|", f)), header=FALSE, sep="|")
  c[,1] <- trimws(c[,1])
  colnames(c) <- c("Key", "Value")
  return(c)
}
