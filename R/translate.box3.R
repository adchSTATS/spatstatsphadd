#' Translate window
#'
#' Function that translate a window of class \code{box3} by some specified vector \code{x}.
#' @param x A vector with 3 entries. must be given in the order \code{x}, \code{y}, and \code{z}.
#' @param win An object of class \code{box3}, that is to be translated.
#' @return An object of class \code{box3}.
#' @importFrom spatstat verifyclass
#' @export
translate.box3 <- function(x, win) {
  stopifnot(verifyclass(win, "box3"))
  stopifnot(is.vector(x) && length(x) == 3 && is.numeric(x))
  win$xrange <- win$xrange + x[1]
  win$yrange <- win$yrange + x[2]
  win$zrange <- win$zrange + x[3]
  win
}
