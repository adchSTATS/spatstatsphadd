% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rKent.R
\name{rKent}
\alias{rKent}
\title{Simulate from the Kent distribution}
\usage{
rKent(n, cons, oval, mean, minor, major)
}
\arguments{
\item{n}{Positive integer. Number of points to be simulated.}

\item{cons}{Positive real number. The consentration parameter. For large values the distribution will be very concentrated.}

\item{oval}{Positive real number. Unimodal if \code{2*oval <= cons }, bimodal otherwise.}

\item{mean}{Three dimensional vector. Must be orthogonal to the minor and major vectors. Will be stadardized.}

\item{minor}{Three dimensional vector. Must be orthogonal to the mean and major vectors. Will be stadardized.}

\item{major}{Three dimensional vector. Must be orthogonal to the mean and minor vectors. Will be stadardized.}
}
\value{
A matrix with the simulated data.
}
\description{
Function for simulating point from a Kent distribution.
}
\author{
Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
}
