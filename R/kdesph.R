#' Spherical kernel density
#' 
#' Function for calculating the dentisty at a location x given in longitudinal and colattitudinal angles
#' @param x A numeric vector of length >= 2. A point on the sphere for which we want to calculate the density.
#' @param kappa A consentration parameter
#' @param obs A dataframe with columns \code{long} and \code{lat}. The observed point pattern on the sphere.
#' @param first A parameter indicating whether the first entry of \code{X} is the longitude or colatitude. 
#' @details This method corresponds to a mixture of Von-Mises Fisher distributions, 
#' with each observed point constituting the mean of a Von-Mises Fisher distribution, all with the same consentration parameter \code{kappa}.
#' @return A numeric of lenght 1. The density at point \code{X}.
#' @author Andreas Christoffersen \email{andreas@math.aau.dk}
#' @export
kdesph <- function(x, kappa, obs, first = "long"){
  if(first == "long"){
    long <- x[1]
    colat <- x[2]
  } 
  if(first ==  "lat"){
    long <- x[2]
    colat <- x[1]
  }
  const <- kappa / (nrow(obs) * 2 * pi * (exp(kappa) - exp(-kappa)))
  out <- const * sum(exp(kappa * (sin(colat) * sin(obs$lat) * cos(long - obs$long) 
                                  + cos(colat)*cos(obs$lat)))) 
  return(out)
}
