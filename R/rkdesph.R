#' Simulate from Spherical kernel density
#'
#' Function for simulating under a spherical kernel density.
#' @param kappa A consentration parameter
#' @param obs A matrix with rows constituting either cartesian or spherical coordinates of the points.
#' If \code{obs} is spicified by spherical coordinates \code{sph_coords} must be TRUE.
#' On the other hand if \code{obs} is spicified by cartesian coordinates \code{sph_coords} must be FALSE.
#' Default is TRUE.
#' The points need not have norm 1.
#' @param nsim The number of point patterns to be simulated.
#' @param np Either a single number indicating the exact number of points in the simulated pattern
#' or "Poisson" in which case the number of points in each simulated pattern will be Poisson distributed
#' with mean equal to the number of points in the observed pattern.
#' @param drop A logical value indicating whether the output should be a \code{\link{sp2}} object of a list of \code{\link{sp2}} objects.
#' This only has an effect if \code{nsim == 1}.
#' @param ncores Number of cores for multiple simulation.
#' @return A \code{\link{sp2}} object or a list of \code{\link{sp2}} object with length equal to \code{nsim}.
#' See also under \code{drop} for further details.
#' @author Andreas Christoffersen \email{andreas@math.aau.dk}
#' @import spherstat genfun parallel
#' @importFrom movMF rmovMF
#' @importFrom stats rpois
#' @export
rkdesph <- function(kappa, obs, nsim = 1, np = "Poisson", drop = TRUE, ncores = 1L){
  stopifnot(verifyclass(obs, "pps"))
  obs_car <- as.matrix(sph2car(as.data.frame(obs$data)))
  if (np == "Poisson"){
    np <- rpois(nsim, npoints(obs))
  }
  out <- mclapply(1:nsim, FUN = function(i) {
    tmp <- as.data.frame(rmovMF(np[i], kappa * obs_car)[, 1:3])
    tmp2 <- do.call(rbind, apply(tmp, 1, car2sph, rep = "sph"))
    pps(x = tmp2$long, y = tmp2$lat, window = obs$window)
  }, mc.cores = ncores)
  if (drop & nsim == 1) {
    out <- out[[1]]
  }
  return(out)
}
