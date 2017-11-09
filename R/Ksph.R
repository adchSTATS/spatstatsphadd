#' A modification of the Ksphere function
#'
#' Function that calculates the inhomogeneous K-functio as well as the lambdavalues required by \code{spherstat}
#' @param X An object of class \code{\link{pps}}
#' @param r Optional. Numeric vector of radii at which K(r) will be evaluated.
#' @param rmax Optional. Maximum value of argument r for which Ksph(r) will be estimated.
#' @param nrval Optional. Number of values of r for which Ksph(r) will be estimated.
#' @param correction Character string or character vector specifying the choise of edge corrections.
#' @param intenss Optional. A function or vector representing the intensity at the observed locations.
#' Only applicable for inhomgeneous point processes.
#' @param ... Additional arguments passed to \code{intenss} if \code{intenss} is a function.
#' @details This function has been implemented since \code{\link{Ksphere}} needs an argument \code{lambdavalues} when calculating the inhomogenious K-function.
#' Hence, when applying the \code{\link{envelope}} function from \code{\link{spatstat}} we need to supply additional information for each point pattern for which \code{K(r)} should be calculated.
#' That is at this point not possible.
#' This function calculates the \code{lambdavalues} and the applies \code{\link{Ksphere}}.
#' The \code{lambdavalues} are calculated based on the function \code{FUN}.
#' @return An object as returned by \code{Ksphere}. An \code{\link{fv}} object.
#' @author Andreas Christoffersen \email{andreas@math.aau.dk}
#' @import spherstat spatstat
#' @export
Ksph <- function(X, r = NULL, rmax = NULL, nrval = 128, correction = c("un", "iso", "rs", "rsm"), intenss = NULL, ...) {
  stopifnot(verifyclass(X, "pps"))
  stopifnot(is.null(intenss) || is.function(intenss) || (is.numeric(intenss) && length(intenss) == npoints(X)))
  if (is.function(intenss)) {
    intenss <- apply(data.frame(X$data$long, X$data$lat), MARGIN = 1, FUN = intenss, ...)
  }
  if (is.null(r)) {
    if (is.null(rmax)) {
      rmax <- pi
    }
    r_vec <- seq(from = 0, to = rmax, length.out = nrval)
  }
  sp2_obj <- sp2(cbind(X$data$lat, X$data$long), win = X$window)
  Ksphere(X = sp2_obj, win = X$window, r = r, lambdavalues = intenss, correction = "iso")
}
