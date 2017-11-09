#' Translation Edge Correction
#'
#' Computes Osher and Stoyans's translation edge correction weights for a 3 dimensional point pattern.
#' The function is only applicable for \code{\link{pp3}} objects observed on a window of class \code{\link{box3}}.
#' @param X An object of class \code{\link{pp3}}, with a window of class \code{\link{box3}}.
#' @param trim option for handling the case otwo points lying on the boundary, in which case the correction factor is \code{Inf}.
#' @details \code{1 / |W cup W_{eta - xi}|}
#' @return A numeric matrix of edge correction weights.
#' @export
edge.Trans.pp3 <- function(X, trim = spatstat.options("maxedgewt")) {
  stopifnot(verifyclass(X, "pp3"))
  stopifnot(verifyclass(X$domain, "box3"))
  dx <- outer(X$data$x, X$data$x, "-")
  dy <- outer(X$data$y, X$data$y, "-")
  dz <- outer(X$data$z, X$data$z, "-")
  width <- diff(X$domain$xrange)
  depth <- diff(X$domain$yrange)
  height <- diff(X$domain$zrange)
  weight <- 1 / ((width - abs(dx)) * (depth - abs(dy)) * (height - abs(dz)))
  if (length(weight) > 0) {
    weight <- pmin.int(weight, trim)
  }
  matrix(weight, ncol = npoints(X))
}
