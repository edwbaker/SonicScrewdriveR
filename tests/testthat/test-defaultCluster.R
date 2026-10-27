#The cluster constructors are stood in for, so that the dispatch between them
#can be checked without starting a worker for every core on the machine.
mockClusters <- function(cores=4L) {
  local_mocked_bindings(
    detectCores = function(...) cores,
    makeCluster = function(spec, ...) structure(list(spec=spec, kind="psock"), class="mock"),
    makeForkCluster = function(nnodes, ...) structure(list(spec=nnodes, kind="fork"), class="mock"),
    .env = parent.frame()
  )
}

test_that("one core is left free for the session itself", {
  mockClusters(cores=4L)
  expect_equal(defaultCluster(fork=FALSE)$spec, 3L)
})

test_that("a single core still gives a cluster of one rather than of none", {
  mockClusters(cores=1L)
  expect_equal(defaultCluster(fork=FALSE)$spec, 1L)
})

test_that("fork=FALSE asks for a cluster that does not fork", {
  mockClusters()
  expect_equal(defaultCluster(fork=FALSE)$kind, "psock")
})

test_that("forking is asked for only where the platform can do it", {
  mockClusters()
  cl <- defaultCluster(fork=TRUE)
  if (.Platform$OS.type == "windows") {
    #Windows has no fork, so the request is quietly ignored rather than failing.
    expect_equal(cl$kind, "psock")
  } else {
    expect_equal(cl$kind, "fork")
  }
})

test_that("the cluster returned can be used and stopped", {
  skip_on_cran()
  skip_if_not_installed("parallel")
  #R CMD check holds a package to two processes, and defaultCluster() asks for
  #one fewer than the machine has, so on all but the smallest machine the
  #cluster cannot be created there and parallel refuses it.
  skip_if(
    isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_", "FALSE"))),
    "R CMD check limits the number of processes a package may start"
  )
  cl <- defaultCluster()
  on.exit(parallel::stopCluster(cl), add=TRUE)
  expect_s3_class(cl, "cluster")
  expect_equal(length(cl), max(1L, parallel::detectCores() - 1L))
  expect_equal(unlist(parallel::clusterEvalQ(cl, 1 + 1))[[1]], 2)
})
