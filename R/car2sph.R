#' Spherical coordinates
#'
#' Transform a 3D vector constituting a cartesian coordinate set to spherical coordinates
#' @param X A numerical vector or matrix whos entries constitute a cartesian coordinate.
#' A vector should have 3 entries and a matrix should have 3 columns.
#' @param deg Logical value indicating whether the output should be in randians or degrees. Radian is default.
#' @param rep Determine the representation of the spherical coordinates.
#' If \code{"sph"} long will range from \code{0} to \code{2 * pi} and lat will range from \code{0} to \code{pi}.
#' The classical representation of spherical coordinates in radians.
#' If \code{"geo"} long will range from \code{-pi} to \code{pi} and lat will range from \code{0} to \code{pi}.
#' The geografical representation in radians.
#' Deafult is "sph".
#' @param unitlength Logical values indicating if the points should be returned with unit length.
#' If TRUE (default) \code{r} will be set to 1.
#' @return A data.frame with 3 columns longitude, latitude and radial distance.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @export
car2sph <- function(X, deg = FALSE, rep = "sph", unitlength = TRUE){
  stopifnot(length(X) == 3 || ncol(X) == 3)
  if(!is.null(dim(X))){
    x <- X[, 1]
    y <- X[, 2]
    z <- X[, 3]
  } else {
    x <- X[1]
    y <- X[2]
    z <- X[3]
  }
  r <- sqrt(x^2 + y^2 + z^2)
  lat <- ifelse(r == 0, 0, acos(ifelse(z / r < -1, -1, ifelse(z / r > 1, 1, z / r))))
  r <- if(unitlength) 1 else r
  if(rep == "sph") {
    long <- atan2(y, x) %% (2 * pi)
  } else if(rep == "geo") {
    long <- atan2(y, x)
  } else stop(paste(sQuote("rep"), "should be", sQuote("sph"), "or", sQuote("geo")))
  if(deg){
    lat <- lat * 180 / pi
    long <- long * 180 / pi
  }
  out = data.frame(long = long, lat = lat, r = r)
  return(out)
}
