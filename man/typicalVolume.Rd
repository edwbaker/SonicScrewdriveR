% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/typicalVolume.R
\name{typicalVolume}
\alias{typicalVolume}
\title{Typical volumes}
\usage{
typicalVolume(thing = NA_character_)
}
\arguments{
\item{thing}{Volume of thing, if missing then returns all volumes}
}
\value{
Typical volume of thing in dBA, or if no thing parameter a data frame of all volumes
}
\description{
Typical volumes of everyday things.
}
\examples{
typicalVolume()
typicalVolume("rocket launch")

}
