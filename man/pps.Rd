% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pps.R
\name{pps}
\alias{pps}
\title{Create point pattern}
\usage{
pps(x, y, z, window = sphwin(), check = TRUE)
}
\arguments{
\item{x}{Vector of longitudinal angles (should range from 0 to 2*pi) or cartesian x coordinates of the data points.}

\item{y}{Vector of latitudinal angles (should range from 0 to pi) or cartesian y coordinates of the data points.}

\item{z}{Vector of cartesian z coordinates. Should be specified only if x and y represent cartesian coordiantes.}

\item{window}{Window of observation, an object of class \code{\link{sphwin}}.}

\item{check}{Logical value indicating wheter to check that al points lie inside the specified window.}
}
\value{
An object of class \code{pps} describing a point pattern on the unit circle.
}
\description{
Creates an object of class \code{pps} representing a point pattern dataset on the unit sphere.
}
\author{
Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
}
