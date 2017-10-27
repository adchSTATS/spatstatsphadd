#' Simulate from Spherical kernel density
#'
#' Function for simulating under a spherical kernel density.
#' @param np Either a single number indicating the exact number of points in the simulated pattern
#' or "Poisson" in which case the number of points in each simulated pattern will be Poisson distributed
#' with mean equal to the number of points in the observed pattern
#' @param kappa A consentration parameter
#' @param obs A matrix with rows constituting either cartesian or spherical coordinates of the points.
#' If \code{obs} is spicified by spherical coordinates \code{sph_coords} must be TRUE.
#' On the other hand if \code{obs} is spicified by cartesian coordinates \code{sph_coords} must be FALSE.
#' Default is TRUE.
#' The points need not have norm 1.
#' @param nsim The number of point patterns to be simulated.
#' @param sph_coords A logical value indicating wether the data supplied are in spherical or cartesian coordinates.
#' @param drop A logical value indicating whether the output should be a \code{\link{sp2}} object of a list of \code{\link{sp2}} objects.
#' This only has an effect if \code{nsim == 1}.
#' @return A \code{\link{sp2}} object or a list of \code{\link{sp2}} object with length equal to \code{nsim}.
#' See also under \code{drop} for further details.
#' @author Andreas Christoffersen \email{andreas@math.aau.dk}
#' @import spherstat genfun
#' @importFrom movMF rmovMF
#' @importFrom stats rpois
#' @export
rkdesph <- function(np, kappa, obs, nsim, sph_coords = TRUE, drop = TRUE){
  if(sph_coords){
    obs <- t(apply(as.data.frame(obs), 1, sph2car))
  }
  out <- list()
  if(np == "Poisson"){
    np <- rpois(nsim, nrow(obs))
  }
  for(i in 1:nsim){
    tmp <- as.data.frame(rmovMF(np[i], kappa * obs)[, 1:3])
    out[[i]] <- sp2(t(apply(tmp, 1, car2sph, rep = "1"))[, c("lat", "long")])
  }
  if(drop & nsim == 1){
    out <- out[[1]]
  }
  return(out)
}
