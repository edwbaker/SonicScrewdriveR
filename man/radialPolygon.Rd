% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/radialPolygon.R
\name{radialPolygon}
\alias{radialPolygon}
\title{Plot a radial polygon}
\usage{
radialPolygon(
  angle1,
  angle2,
  radius1,
  radius2,
  col = "grey",
  border = NA,
  rot = -pi,
  angleinc = 0.01,
  reverse = TRUE,
  ...
)
}
\arguments{
\item{angle1}{Angles for the inner line}

\item{angle2}{Angles for the outer line}

\item{radius1}{Radii for the inner line}

\item{radius2}{Radii for the outer line}

\item{col}{Colour of the polygon}

\item{border}{Border colour (see polygon() for details)}

\item{rot}{Rotation of the plot, defaults to pi to match dielPlot() and yearlyPlot()}

\item{angleinc}{The angular increment in radians for calculating circular lines}

\item{reverse}{If FALSE plots in an anti-clockwise direction}

\item{...}{Other parameters passed to polygon()}
}
\description{
Used to plot sectors, annuli and horizons on a dielPlot() or yearlyPlot(). The polygon
has an inner and outer horizon - which can be set to a fixed radius or a vector.
}
