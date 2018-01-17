#' Simulate from the Kent distribution
#'
#' Function for simulating point from a Kent distribution.
#' @param n Positive integer. Number of points to be simulated.
#' @param cons Positive real number. The consentration parameter. For large values the distribution will be very concentrated.
#' @param oval Positive real number. Unimodal if \code{2*oval <= cons }, bimodal otherwise.
#' @param mean Three dimensional vector. Must be orthogonal to the minor and major vectors. Will be stadardized.
#' @param minor Three dimensional vector. Must be orthogonal to the mean and major vectors. Will be stadardized.
#' @param major Three dimensional vector. Must be orthogonal to the mean and minor vectors. Will be stadardized.
#' @return A matrix with the simulated data.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @importFrom Directional rotation rfb
#' @export
rKent <- function(n, cons, oval, mean, minor, major) {
  if (!(crossprod(mean, minor) == 0 &&
       crossprod(mean, major) == 0 &&
       crossprod(minor, major) == 0)) {
    stop("Mean, minor and major does not constitute a ortogonal basis.")
  }
  mean <- mean/sqrt(sum(mean^2))
  minor <- minor/sqrt(sum(minor^2))
  major <- major/sqrt(sum(major^2))
  A1 <- rotation(mean, c(0, 1, 0)) %*% cbind(mean, minor, major)
  A2 <- A1 %*% diag(c(0, 1, -1)) %*% t(A1)
  rfb(n, cons, mean, oval * A2)
}
