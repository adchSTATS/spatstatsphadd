#' Cartesian coordinates
#'
#' Transform a spherical coordinates set to cartesian coordinates
#' @param X A numerical vector or matrix whos entries constitute sperical coordinates.
#' A vector should have 2 or 3 entries and a matrix should have 2 or 3 columns.
#' If the input has 2 entries or columns it will be assumed that the radial distance i 1.
#' The order of the vector entries (matrix columns) should be longitude, latitude and radial distance (if applicable).
#' @param deg Logical value indicating whether the input is in randians or degrees. Radian is default.
#' @param rep Determine the representation of the spherical coordinates. 
#' If \code{"sph"} long will range from \code{0} to \code{2 * pi} and lat will range from \code{0} to \code{pi}.
#' The classical representation of spherical coordinates in radians.
#' If \code{"geo"} long will range from \code{-pi} to \code{pi} and lat will range from \code{0} to \code{pi}.
#' The geografical representation in radians.
#' Deafult is "sph".
#' @return A \code{data.frame} with 3 columns representing the cartesian coordinate set.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @export
sph2car <- function(X, deg = FALSE, rep = "sph") {
  stopifnot(length(X) == 3 || ncol(X) == 3 || length(X) == 2 || ncol(X) == 2)
  r_specified <- length(X) == 3 || ncol(X) == 3
  if(!is.null(dim(X))){
    long <- X[, 1]
    lat <- X[, 2]
    r <- if(r_specified) X[, 3] else 1
  } else {
    long <- X[1]
    lat <- X[2]
    r <- if(r_specified) X[3] else 1
  }
  if(deg){
    long <- long * pi / 180
    lat <- lat * pi / 180
  }
  if(rep == "geo") {
    long %% (2 * pi) 
  } else if(rep != "sph") {
    stop(paste(sQuote("rep"), "should be", sQuote("sph"), "or", sQuote("geo")))
  }
  sinlat <- sin(lat)
  out <- r * cbind(sinlat * cos(long), 
                   sinlat * sin(long), 
                   cos(lat))
  out <- as.data.frame(ifelse(abs(out) < .Machine$double.eps, 0, out))
  colnames(out) <- c("x", "y", "z")
  return(out)
}
