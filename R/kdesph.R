#' Spherical kernel density
#'
#' Function for calculating the dentisty at a location x given in longitudinal and colattitudinal angles
#' @param x A numeric vector of length 2 (may be longer but only the first two entries are used).
#' Should represent a point on the sphere for which we want to calculate the density.
#' Given by longitude (should range from 0 to 2 * pi) and colatitude (should range from 0 to pi).
#' @param kappa A consentration parameter
#' @param obs A dataframe with columns \code{long} and \code{lat}.
#' The observed point pattern on the sphere.
#' Given by longitude (should range from 0 to 2 * pi) and colatitude (should range from 0 to pi).
#' @param first A parameter indicating whether the first entry of \code{X} is the longitude or colatitude.
#' @details This method corresponds to a mixture of Von-Mises Fisher distributions,
#' with each observed point constituting the mean of a Von-Mises Fisher distribution, all with the same consentration parameter \code{kappa}.
#' @return A numeric of lenght 1. The density at point \code{X}.
#' @author Andreas Christoffersen \email{andreas@math.aau.dk}
#' @import spatstat
#' @export
kdesph <- function(x, kappa, obs, first = "long"){
  stopifnot(is.numeric(x) && is.vector(x))
  stopifnot(kappa >= 0)
  stopifnot(verifyclass(obs, "pps"))

  if(first == "long"){
    long <- x[1]
    colat <- x[2]
  } else if(first ==  "lat"){
    long <- x[2]
    colat <- x[1]
  }
  stopifnot(long >= 0 && long <= 2 * pi)
  stopifnot(colat >= 0 && colat <= pi)

  dat_long <- obs$data$long
  dat_lat <- obs$data$lat
  stopifnot(all(dat_long >= 0) && all(dat_long <= 2 * pi))
  stopifnot(all(dat_lat >= 0) && all(dat_lat <= pi))

  const <- kappa / (length(dat_long) * 2 * pi * (exp(kappa) - exp(-kappa)))
  const * sum(exp(kappa * (sin(colat) * sin(dat_lat) * cos(long - dat_long) + cos(colat)*cos(dat_lat))))
}
