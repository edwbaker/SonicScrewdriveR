#The cluster constructors are stood in for, so that the dispatch between them
#can be checked without starting a worker for every core on the machine. The
#stream the workers are given is stood in for with them, as the cluster they
#would be given it on is not one parallel would accept, and recorded so that
#the call can be checked.
mockClusters <- function(cores=4L) {
  record <- new.env(parent=emptyenv())
  record$called <- FALSE
  local_mocked_bindings(
    detectCores = function(...) cores,
    makeCluster = function(spec, ...) structure(list(spec=spec, kind="psock"), class="mock"),
    makeForkCluster = function(nnodes, ...) structure(list(spec=nnodes, kind="fork"), class="mock"),
    clusterSetRNGStream = function(cl, iseed=NULL) {
      record$called <- TRUE
      record$cl <- cl
      record$iseed <- iseed
      invisible(NULL)
    },
    .env = parent.frame()
  )
  invisible(record)
}

#A cluster of real workers is needed to show what numbers they draw, and
#R CMD check holds a package to two processes.
skipUnlessClusterAllowed <- function() {
  skip_on_cran()
  skip_if_not_installed("parallel")
  skip_if(
    isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_", "FALSE"))),
    "R CMD check limits the number of processes a package may start"
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

test_that("a machine whose cores cannot be counted gives a cluster of one", {
  #detectCores() is documented to return NA where it cannot work out how many
  #cores there are, and NA - 1L is NA rather than something max() will discard.
  mockClusters(cores=NA_integer_)
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

test_that("the cluster is given a random number stream", {
  #Without one each worker seeds itself from its own entropy, so a windowed
  #analysis using randomness gave different numbers on every run.
  stream <- mockClusters()
  cl <- defaultCluster(fork=FALSE)
  expect_true(stream$called)
  expect_equal(stream$cl, cl)
  #No seed given leaves the streams to be drawn from the session's own state,
  #which is what makes set.seed() before the call reach the workers.
  expect_null(stream$iseed)
})

test_that("a seed given is the seed the streams are drawn from", {
  stream <- mockClusters()
  defaultCluster(fork=FALSE, seed=42)
  expect_equal(stream$iseed, 42)
})

test_that("the cluster returned can be used and stopped", {
  skipUnlessClusterAllowed()
  #defaultCluster() asks for one fewer worker than the machine has, so on all
  #but the smallest machine that is more than R CMD check allows.
  cl <- defaultCluster()
  on.exit(parallel::stopCluster(cl), add=TRUE)
  expect_s3_class(cl, "cluster")
  expect_equal(length(cl), max(1L, parallel::detectCores() - 1L, na.rm=TRUE))
  expect_equal(unlist(parallel::clusterEvalQ(cl, 1 + 1))[[1]], 2)
})

test_that("each worker draws from a stream of its own", {
  skipUnlessClusterAllowed()
  #Two workers whatever the machine has, so that a cluster can be started
  #several times in a test and is the same size wherever the test is run.
  local_mocked_bindings(detectCores = function(...) 3L)
  cl <- defaultCluster(fork=FALSE)
  on.exit(parallel::stopCluster(cl), add=TRUE)
  drawn <- unlist(parallel::clusterEvalQ(cl, stats::runif(1)))
  #Seeding every worker alike would be reproducible and still wrong, as each
  #would then return the same numbers as the others.
  expect_equal(length(unique(drawn)), 2L)
  expect_equal(
    unlist(parallel::clusterEvalQ(cl, RNGkind()[[1]])),
    rep("L'Ecuyer-CMRG", 2L)
  )
})

test_that("a windowed analysis using randomness repeats under the same seed", {
  skipUnlessClusterAllowed()
  local_mocked_bindings(detectCores = function(...) 3L)
  wave <- tuneR::sine(440, duration=400, samp.rate=4000, bit=16, pcm=TRUE)
  #Randomness in the window, as generateTimeMask(method="random") and
  #generateNoise() use it, reduced to a number per window.
  sampled <- function(...) {
    cl <- defaultCluster(fork=FALSE, ...)
    on.exit(parallel::stopCluster(cl), add=TRUE)
    unlist(windowing(
      wave, window.length=100, window.overlap=0, cluster=cl,
      FUN=function(w, start, window.length, ...) sum(sample(as.numeric(w@left), 10))
    ))
  }
  set.seed(42)
  first <- sampled()
  set.seed(42)
  again <- sampled()
  set.seed(7)
  other <- sampled()
  expect_equal(length(first), 4L)
  expect_equal(first, again)
  #A result that cannot be told from the next seed's would pass the test above
  #while reporting nothing of the randomness at all.
  expect_false(isTRUE(all.equal(first, other)))
  #A seed given to the cluster does the same without touching the session's.
  expect_equal(sampled(seed=42), sampled(seed=42))
  expect_false(isTRUE(all.equal(sampled(seed=42), sampled(seed=7))))
})

test_that("making a cluster leaves the session's own random numbers alone", {
  skipUnlessClusterAllowed()
  local_mocked_bindings(detectCores = function(...) 3L)
  set.seed(42)
  cl <- defaultCluster(fork=FALSE)
  after <- stats::runif(3)
  parallel::stopCluster(cl)
  #The streams are drawn from this state, so taking them must not spend it or
  #leave the session's generator changed.
  set.seed(42)
  expect_equal(after, stats::runif(3))
})
