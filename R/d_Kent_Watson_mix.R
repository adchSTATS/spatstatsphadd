#' Mixture Density Function of Kent and Watson
#'
#' Density function for the mixture of the Kent and Watson distributions.
#' @param x An object of class \code{\link{pps}}.
#' @param mean_dir_Kent Numeric vector. See \link{dKent}
#' @param minor_dir Numeric vector. See \link{dKent}
#' @param major_dir Numeric vector. See \link{dKent}
#' @param cons_Kent Numeric value. The consentration parameter for the Kent distribution. See \link{dKent}
#' @param oval_Kent Numeric value. The ovalness parameter for the Kent distribution. See \link{dKent}
#' @param mean_dir_Watson Numeric vector. See \link{dWatson}
#' @param cons_Watson Numeric value. The consentration parameter for the Watson distribution. See \link{dWatson}
#' @param p_Kent Numeric value between 0 and 1. The probability of observing a point from the Kent distribution.
#' @export
d_Kent_Watson_mix <- function(x,
                              mean_dir_Kent, minor_dir, major_dir, cons_Kent, oval_Kent,
                              mean_dir_Watson, cons_Watson,
                              p_Kent) {
  stopifnot("pps" %in% class(x))
  p_Kent * dKent(x, kappa=cons_Kent, beta=oval_Kent, meandir=mean_dir_Kent, minordir=minor_dir, majordir=major_dir) +
    (1 - p_Kent) * dWatson(x, kappa=cons_Watson, alpha=mean_dir_Watson)
}
