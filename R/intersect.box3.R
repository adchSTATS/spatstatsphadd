#' Get intersection of two windows
#'
#' Function that gets the intersection of two windows of class \code{box3}.
#' @param A,B An object of class \code{box3}.
#' @param fatal Logical. Determines what happens if the intersection is empty.
#' @return A window (object of class \code{\link{box3}}) or possibly \code{NULL}.
#' @importFrom spatstat.utils intersect.ranges
#' @export
intersect.box3 <- function(A, B, fatal = TRUE){
  stopifnot(verifyclass(A, "box3"))
  stopifnot(verifyclass(B, "box3"))
  if(compatible(unitname(A), unitname(B))) {
    unit <- unitname.box3(A)
  } else {
    warning("The windows have unequal units, which this function cannot handle.")
    unit <- NULL
  }
  xr <- try(intersect.ranges(A$xrange, B$xrange, fatal = fatal), silent = TRUE)
  yr <- try(intersect.ranges(A$yrange, B$yrange, fatal = fatal), silent = TRUE)
  zr <- try(intersect.ranges(A$zrange, B$zrange, fatal = fatal), silent = TRUE)
  if(any(class(xr) == "try-error", class(yr) == "try-error", class(zr) == "try-error")) {
    stop("Intersection is empty")
  }
  if (!fatal && (is.null(xr) || is.null(yr) || is.null(zr))) {
    return(NULL)
  }
  box3(xrange = xr, yrange = yr, zrange = zr, unitname = unit)
}
