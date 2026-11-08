# Echemes in a cricket song
# -------------------------
# An experiment. It is not part of the package, and sourcing it over a loaded
# sonicscrewdriver is the only way it is meant to be run.
#
# An echeme is a group of pulses set off from the next group by a longer
# silence, one level above the pulse in the song of a cricket or a bush
# cricket. It is found here from the pulse distances alone, so it works on the
# output of any detector in the package.
#
# The hard part is not placing the boundaries. It is deciding whether there are
# any. A song that runs on without grouping, such as the trill of many
# Oecanthus, has no echemes at all, and a method that simply splits at the
# largest gap will always report some: measured on 27 recordings of Oecanthus
# pictus, splitting without that test gave between 1 and 468 pulses per echeme
# for the one species, a spread of 8.9 octaves. The same test over the same
# recordings reports no echemes for all 27, and the species that do group give
# 0.8 octaves at worst.

#' Echemes
#'
#' Groups the pulses a detector reported into echemes, where the song has them.
#'
#' @details
#' An echeme is a run of pulses separated from the next run by a longer
#' silence. The distances between successive pulses are therefore expected to
#' fall into two groups: the short ones within an echeme and the long ones
#' between them. This function finds the division between those two groups that
#' best separates them, and reports the runs of pulses it implies.
#'
#' The division is made on the logarithm of the distance, because the two kinds
#' of silence differ by a factor rather than by a number of samples, and it is
#' placed where the variance between the two groups is greatest. `separation`
#' reports the share of the variance in the log distances that the division
#' accounts for, from 0 to 1, and `ratio` the factor between the mean distance
#' within an echeme and the mean distance between echemes.
#'
#' Not every song has echemes. A continuous trill has one kind of silence, not
#' two, and dividing its distances in the same way splits a run of pulses at
#' whichever silence happens to be longest, which says nothing about the song.
#' Two conditions therefore have to hold before any echeme is reported:
#'
#' - an echeme must hold at least `min.pulses` pulses, as a group of one pulse
#'   is a pulse and not a group of them, and
#' - `separation` must be at least `min.separation`, so that the distances
#'   really do fall into two groups rather than one.
#'
#' Where either fails, no echeme is reported and `onsets` and `offsets` are
#' empty. That is an answer about the song and not a failure: the diagnostics
#' are still returned, so the reason can be seen. `separation` of 0 means the
#' distances carry no grouping at all.
#'
#' Echemes follow the same boundary convention as the pulses they are made of,
#' described under [pulseDetection()]: an onset is the first sample of the first
#' pulse of the echeme, and an offset the first sample after its last pulse.
#'
#' A pulse the recording ends part way through has an onset and no offset. Its
#' length is unknown, so no silence can be measured from it, and it is left out
#' of every echeme rather than ending one at a sample that is not known.
#'
#' @param pulses The result of a [pulseDetection()], holding onsets and offsets.
#' @param min.pulses The fewest pulses a group must hold to be an echeme.
#' @param min.separation The least share of the variance in the log distances
#'   that the division between the two groups must account for, from 0 to 1.
#' @return A list holding the onset and offset of each echeme, the number of
#'   pulses in each, the `separation` and `ratio` described above, and the
#'   `threshold` distance in samples above which a silence was taken to lie
#'   between echemes rather than within one.
#' @examples
#' #A second of song holding ten echemes of four pulses each.
#' x <- numeric(44100)
#' for (o in as.vector(outer(c(0, 250, 500, 750), seq(0, 9)*4410, "+"))) {
#'   x[o + seq_len(200)] <- 1
#' }
#' w <- tuneR::Wave(x, samp.rate=44100, bit=32, pcm=FALSE)
#' e <- echemes(pulseDetection(w, method="threshold"))
#' length(e$onsets)
#' e$pulses
#' e$separation
#'
#' #The same pulses evenly spaced are a trill, and hold no echemes.
#' y <- numeric(44100)
#' for (o in seq(0, by=1050, length.out=40)) {
#'   y[o + seq_len(200)] <- 1
#' }
#' trill <- tuneR::Wave(y, samp.rate=44100, bit=32, pcm=FALSE)
#' length(echemes(pulseDetection(trill, method="threshold"))$onsets)
#'
echemes <- function(
  pulses,
  min.pulses=2,
  min.separation=0.8
) {
  .validatePositiveCount(min.pulses, "min.pulses")
  if (!is.numeric(min.separation) || length(min.separation) != 1 ||
      is.na(min.separation) || min.separation < 0 || min.separation > 1) {
    stop("min.separation must be a single number between 0 and 1, as it is a share of a variance.")
  }
  if (is.null(pulses$offsets)) {
    stop("pulses must hold offsets as well as onsets, as the silence between two pulses begins where a pulse ends.")
  }
  #pulseStats() pairs the onsets with the offsets and refuses a list where the
  #two describe different pulses, so the distances below are known to run from
  #the end of one pulse to the start of the next.
  distances <- pulseStats(pulses)$distances
  paired <- min(length(pulses$onsets), length(pulses$offsets))
  onsets <- pulses$onsets[seq_len(paired)]
  offsets <- pulses$offsets[seq_len(paired)]

  none <- function(separation=NA_real_, ratio=NA_real_, threshold=NA_real_) {
    list(
      "onsets" = onsets[0],
      "offsets" = offsets[0],
      "pulses" = integer(0),
      "separation" = separation,
      "ratio" = ratio,
      "threshold" = threshold
    )
  }

  #Dividing a set in two needs at least two on each side of the division, so
  #four distances, which is five pulses. Fewer cannot show that a song groups.
  if (length(distances) < 4) {
    return(none())
  }

  split <- .echemeSplit(distances)
  if (is.null(split)) {
    #Every distance the same is a song with one kind of silence, so there is
    #nothing for a division to separate.
    return(none(separation=0))
  }
  if (split$separation < min.separation) {
    return(none(separation=split$separation, ratio=split$ratio, threshold=split$threshold))
  }

  #A distance longer than the division lies between echemes, so the pulse after
  #it opens a new one. The first pulse always opens the first.
  group <- as.integer(cumsum(c(1, distances > split$threshold)))
  #tabulate() counts by the group number itself. table() would coerce the
  #numbers to text and order them as text, so echeme 10 would be counted before
  #echeme 2 and the counts would no longer line up with the boundaries below.
  counts <- tabulate(group, nbins=max(group))
  #The pulses per echeme is taken as the median rather than the mean, so that
  #one run broken by a missed pulse does not decide whether the song groups.
  if (stats::median(counts) < min.pulses) {
    return(none(separation=split$separation, ratio=split$ratio, threshold=split$threshold))
  }

  first <- which(!duplicated(group))
  last <- c(first[-1] - 1, length(group))
  return(list(
    "onsets" = onsets[first],
    "offsets" = offsets[last],
    "pulses" = counts,
    "separation" = split$separation,
    "ratio" = split$ratio,
    "threshold" = split$threshold
  ))
}

#' Divide pulse distances into the short ones and the long ones
#'
#' Places the division where the variance between the two groups is greatest,
#' which is Otsu's method in one dimension, applied to the logarithm of the
#' distance because a silence between echemes is a multiple of a silence within
#' one rather than a fixed number of samples longer.
#'
#' Two pulses can touch, leaving a distance of no samples, which has no
#' logarithm. The division is therefore made on log(distance + 1), which orders
#' the distances the same way and keeps a touching pair in the short group where
#' it belongs. Leaving those distances out instead lost the whole short group
#' from a song whose pulses all touch, and no division could then be found at
#' all.
#'
#' @param distances The distances between successive pulses, in samples.
#' @return A list of the threshold in samples, the share of the variance the
#'   division accounts for, and the factor between the two group means; or NULL
#'   where the distances cannot be divided.
#' @noRd
.echemeSplit <- function(distances) {
  d <- distances[is.finite(distances) & distances >= 0]
  x <- sort(log1p(d))
  n <- length(x)
  if (n < 4) {
    return(NULL)
  }
  #Every distance the same leaves no variance for a division to account for, and
  #the share below would be 0/0.
  total <- sum((x - mean(x))^2)
  if (total == 0) {
    return(NULL)
  }
  #The division is tried between each neighbouring pair. Taking the two group
  #means from the running sum keeps this one pass rather than one per candidate.
  k <- seq_len(n - 1)
  running <- cumsum(x)
  short <- running[k]/k
  long <- (running[n] - running[k])/(n - k)
  #The variance between the two groups, weighted by their sizes. Maximising it
  #is the same as minimising the variance within them, as the two sum to the
  #total.
  between <- k*(n - k)/n*(long - short)^2
  best <- which.max(between)
  #Halfway between the two distances the division separates, so that no observed
  #distance sits exactly on it.
  threshold <- expm1((x[best] + x[best + 1])/2)
  #The factor is taken from the distances themselves rather than from the logs
  #they were divided on, so that it is the factor between the two silences as
  #measured and not between those silences plus a sample.
  sorted <- sort(d)
  shortMean <- mean(sorted[seq_len(best)])
  return(list(
    "threshold" = threshold,
    "separation" = between[best]/total,
    "ratio" = mean(sorted[seq.int(best + 1, n)])/shortMean
  ))
}
