#' Convert pps object to sp2 object
#'
#' Function that converts an object of class \code{\link{pps}} to an object of class \code{\link{sp2}}.
#' @param X An object of class \code{\link{pps}}.
#' @import spatstat spherstat
#' @export
pps2sp2 <- function(X) {
  stopifnot(verifyclass(X, "pps"))
  sp2(data.frame(X$data$lat, X$data$long), win = X$window)
}
