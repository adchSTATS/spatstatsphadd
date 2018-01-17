#' Simulate Watson distribution
#'
#' Function for simulating from a specified Watson distribution
#' @param n Positive integer. The number of points to be simulated.
#' @param conc real number. The concentration parameter if negative it is a girdle distribution, if positive it is a bimodal distribution.
#' @param mean A real three dimensional vector constituting the mean direction of the distribution to be simulated.
#' @return A matrix with the simulated data.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @importFrom Directional rbingham
#' @export
rWatson <- function(n, conc, mean) {
  rbingham(n, -conc * diag(mean))
}
