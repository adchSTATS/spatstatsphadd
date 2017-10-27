#' Kent distribution density
#'
#' Function for calculating the dentisty at a location x given in cartesian coordinates
#' @param x A numeric vector of length 3 or a matrix with 3 columns. 
#' constituting the three dimensional coordinates of a point on the sphere.
#' @param kappa A non-negative numeric value. The consentration parameter
#' @param beta A numeric value. The ovalness parameter.
#' @param meandir A three dimensional vector. Determines where the density will be consentrated.
#' Default will be the northpole.
#' @param minordir A three dimensional vector. Determines the axis that will be the minor axis.
#' Default will be the y-axis.
#' @param majordir A three dimensional vector. Determines the axis that will be the major axis.
#' Default will be the x-axis.
#' @details If \code{2 * beta < kappa} then the distribution is a generalisation of the normal distribution to the sphere.
#' The distribution is a extension of the von-Mises Fisher distribution (\code{beta = 0}). 
#' \code{(meandir, minordir, majordir)} is supposed to be a orthogonal triple of unit vectors.
#' @return A numeric vector of length corresponding to the number of rows in x. 
#' The density at the points \code{X}.
#' @author Andreas Christoffersen \email{andreas@math.aau.dk}
#' @export
dKent <- function(x, kappa, beta, meandir = NULL, minordir = NULL, majordir = NULL) {
  const <- if(kappa == 0 & beta == 0) {
    4 * pi
  } else if(kappa == 0) {
    stop("kappa may only be zero when beta is zero")
  } else if(kappa > 0 & beta == 0) {
    4 * pi / kappa * sinh(kappa)
  } else if(kappa > 2 * beta) {
    2 * pi * exp(kappa) / sqrt(kappa^2 - 4 * beta^2)
  } else {
    j = 0:170
    c <- 2 * pi * sqrt(2 / kappa)
    f1 <- gamma(j + 0.5)/gamma(j + 1)
    f2 <- (2 * beta / kappa)^(2 * j)
    f2 <- f2[f2 != Inf]
    f3 <- suppressWarnings(besselI(kappa, 2 * j + 0.5))
    len <- length(f2)
    if(len < 171){
      f1 <- f1[1:len]
      f3 <- f3[1:len]
    }
    c * sum(f1 * f2 * f3)
  }
  if(is.null(meandir)){
    meandir <- c(0, 0, 1)
  }
  if(is.null(majordir)){
    majordir <- c(1, 0, 0)
  }
  if(is.null(minordir)){
    minordir <- c(0, 1, 0)
  }
  if(is.vector(x)){
    x <- t(x)
  }
  out <- exp(kappa * drop(tcrossprod(meandir, x)) 
             + beta * (drop(tcrossprod(majordir, x))^2 
                       - drop(tcrossprod(minordir, x))^2)) / const
  return(out)
}