% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseFilename.R
\name{parseFilename}
\alias{parseFilename}
\title{Parse a filename}
\usage{
parseFilename(file, format = NULL, timezone = NULL)
}
\arguments{
\item{file}{A filename (or list of filenames).}

\item{format}{Optionally force a given format (see Details). If NULL (default)
an attempt is made to automatically detect the format for each file. If "match"
and a list of filenames is given then an attempt will be made to find a format
that matches all files. This may give incorrect results if the filename is
ambiguous (see Details).}

\item{timezone}{Optionally set a timezone.}
}
\value{
A list of file, type of match, datetime.
\cr\cr
It is possible to determine additional properties from some files, these will
be added to the list.
}
\description{
Attempts to extract meaningful information from a filename, typically the
date and time a recording started.
}
\details{
\subsection{Determining the format}{

It is sometimes impossible to accurately determine the format of
a filename, e.g. when an eight-digit 'AudioMoth HEX' only contains numbers it
could be confused with a YYYYMMDD format. If a list of filenames is given
and the "match" format is specified then an effort will be made to determine
the most likely format that applies to all filenames.
}

\subsection{Supported formats}{
\itemize{
\item \strong{AudioMoth} - The newer format for AudioMoth devices consists of a
standard YYYYMMDD_HHMMSS.wav format. Specifying 'AudioMoth' forces a call
to the \code{audiomoth()} function from the \code{seewave} package
\insertCite{seewave2008}{sonicscrewdriver}.
\item \strong{AudioMoth HEX} - Older format for AudioMoth devices consisting of eight
hexadecimal characters. Conversion is handled by a call to
\code{seewave::audiomoth()}.
\item \strong{timestamp} - A standard date-time format. Uses the R standard origin of
1970-01-01 00:00:00 UTC.
\item \strong{Wildlife Acoustics SM2} - Can also be used for Wildlife Acoustics SM4
devices. Conversion is handled by a call to \code{seewave::songmeter()}.
\item \strong{Wildlife Acoustics SM3} - Conversion is handled by a call to
\code{seewave::songmeter()}.
\item \strong{YYYYMMDD_HHMMSS} - A standard date-time format.
}
}
}
\examples{
parseFilename("5E90A4D4.wav")

}
\references{
\insertAllCited{}
}
