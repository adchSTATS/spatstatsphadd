#' Watson distribution density
#' 
#' Function for calculating the dentisty at a location x given in cartesian coordinates.
#' @param x A numeric vector of length 3 or a matrix with 3 columns. constituting the three dimensional coordinates of a point on the sphere.
#' @param kappa A numeric value. The consentration parameter. May be negative.
#' @param alpha A numeric vector of lengthe 3. Determines the axis at which there will be rotationally symmetry.
#' @details If \code{kappa < 0} the distribution will be an equatorial girdle.
#' If \code{kappa > 0} the distribution will be bimodal around the poles \code{alpha} and \code{-alpha}.
#' @return A numeric vector of length corresponding to the number of rows in x. 
#' The density at the points \code{X}.
#' @author Andreas Christoffersen \email{andreas@math.aau.dk}
#' @importFrom stats integrate pnorm
#' @export
dWatson <- function (x, kappa, alpha = NULL) {
  if (is.null(alpha)) {
    alpha <- c(0, 1, 0)
  }
  c <- if(kappa == 0) {
    4 * pi
  } else if(kappa < 0) {
    4 * pi * sqrt(-pi / kappa) * (pnorm(1, sd = sqrt(-1 / (2 * kappa))) - pnorm(0, sd = sqrt(-1 / (2 * kappa))))
  } else if(kappa > 0) {
    4 * pi * integrate(function(x){exp(kappa * x^2)}, 0, 1)$value
  }
  if(is.vector(x)){
    x <- t(x)
  }
  out <- exp(kappa * drop(tcrossprod(alpha, x))^2) / c
  return(out)
}