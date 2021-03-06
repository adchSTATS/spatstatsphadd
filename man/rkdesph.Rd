% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rkdesph.R
\name{rkdesph}
\alias{rkdesph}
\title{Simulate from Spherical kernel density}
\usage{
rkdesph(kappa, obs, nsim = 1, np = "Poisson", drop = TRUE, ncores = 1L)
}
\arguments{
\item{kappa}{A consentration parameter}

\item{obs}{A matrix with rows constituting either cartesian or spherical coordinates of the points.
If \code{obs} is spicified by spherical coordinates \code{sph_coords} must be TRUE.
On the other hand if \code{obs} is spicified by cartesian coordinates \code{sph_coords} must be FALSE.
Default is TRUE.
The points need not have norm 1.}

\item{nsim}{The number of point patterns to be simulated.}

\item{np}{Either a single number indicating the exact number of points in the simulated pattern
or "Poisson" in which case the number of points in each simulated pattern will be Poisson distributed
with mean equal to the number of points in the observed pattern.}

\item{drop}{A logical value indicating whether the output should be a \code{\link{sp2}} object of a list of \code{\link{sp2}} objects.
This only has an effect if \code{nsim == 1}.}

\item{ncores}{Number of cores for multiple simulation.}
}
\value{
A \code{\link{sp2}} object or a list of \code{\link{sp2}} object with length equal to \code{nsim}.
See also under \code{drop} for further details.
}
\description{
Function for simulating under a spherical kernel density.
}
\author{
Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
}
