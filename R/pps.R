#' Create point pattern
#'
#' Creates an object of class \code{pps} representing a point pattern dataset on the unit sphere.
#' @param x Vector of longitudinal angles (should range from 0 to 2*pi) or cartesian x coordinates of the data points.
#' @param y Vector of latitudinal angles (should range from 0 to pi) or cartesian y coordinates of the data points.
#' @param z Vector of cartesian z coordinates. Should be specified only if x and y represent cartesian coordiantes.
#' @param window Window of observation, an object of class \code{\link{sphwin}}.
#' @param check Logical value indicating wheter to check that al points lie inside the specified window.
#' @return An object of class \code{pps} describing a point pattern on the unit circle.
#' @author Andreas Dyreborg Christoffersen \email{andreas@math.aau.dk}
#' @import spherstat spatstat
#' @export
pps <- function(x, y, z, window = sphwin(), check = TRUE) {
  stopifnot(verifyclass(window, "sphwin"))
  if (missing(x) || missing(y)) {
    long <- numeric(0)
    lat <- numeric(0)
    n <- 0
  } else if (missing(z)) {
    stopifnot(is.numeric(x) & is.vector(x))
    stopifnot(is.numeric(y) & is.vector(y))
    nx <- length(x)
    ny <- length(y)
    stopifnot(nx == ny)
    long <- x %% (2 * pi)
    lat <- y
    n <- nx
    stopifnot(long >= 0 && long < 2*pi)
    stopifnot(lat >= 0 && lat < pi)
  } else if (!missing(z)) {
    stopifnot(is.numeric(x) & is.vector(x))
    stopifnot(is.numeric(y) & is.vector(y))
    stopifnot(is.numeric(z) & is.vector(z))
    nx <- length(x)
    ny <- length(y)
    nz <- length(z)
    stopifnot(nx == ny || nx == nz || ny == nz)
    sphcoord <- car2sph(cbind(x, y, z), deg = FALSE, rep = "sph", unitlength = TRUE)
    long <- sphcoord$long
    lat <- sphcoord$lat
    n <- nx
  }
  if (check == TRUE) {
    sp2obj <- sp2(matrix(c(long, lat), ncol = 2))
    inside <- in.W(points = sp2obj, win = window)
    n_outside <- sum(!inside)
    if(n_outside > 0) warning(paste(n_outside, "point(s) were removed due to not being contained in the specified window"))
    n <- n - n_outside
    long <- long[inside]
    lat <- lat[inside]
  }
  out <- ppx(data = data.frame(long = long, lat = lat, r = rep.int(1, n)),
             domain = window,
             simplify = FALSE)
  pps <- list(data = out$data, window = out$domain, n = n)
  class(pps) <- c("pps", class(out))
  return(pps)
}
