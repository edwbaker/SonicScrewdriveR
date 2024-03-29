% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TaggedWave.R
\name{tagWave}
\alias{tagWave}
\title{Tag a Wave or WaveMC object}
\usage{
tagWave(w, origin = "user")
}
\arguments{
\item{w}{A \code{Wave} or \code{WaveMC} object (or list of such objects).}

\item{origin}{The origin of the object (default "user").}
}
\value{
A \code{TaggedWave} or \code{TaggedWaveMC} object (or list of such objects).
}
\description{
This function takes a \code{Wave}/\code{WaveMC} object (or a list of such objects) and
returns a corresponding tagged version (\code{TaggedWave} or \code{TaggedWaveMC}).
}
