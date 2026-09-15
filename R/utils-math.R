# sinusoidal_interval_mean ----
#' Sinusoidal Interval Mean
#'
#' Computes the mean value of a sinusoidal circadian function over a time
#' interval \code{[t1, t2]}, as used in the Chain et al. (2013) hierarchical
#' JAGS model fitted by \code{\link{chain_priors_from_data}}.
#'
#' The formula is:
#' \deqn{A \cdot \frac{12}{\pi (t_2 - t_1)} \left[\sin\!\left(\frac{\pi}{12} t_2 + \phi\right) - \sin\!\left(\frac{\pi}{12} t_1 + \phi\right)\right]}
#'
#' where \eqn{\phi} is a raw phase offset in radians (consistent with the
#' \code{dunif(0, 2 * pi)} prior on \code{phi0}).
#'
#' @param A numeric. Amplitude parameter (scalar or vector of MCMC samples).
#' @param t1 numeric. Interval start time in hours.
#' @param t2 numeric. Interval end time in hours.
#' @param phi numeric. Phase offset in radians (scalar or vector of MCMC samples).
#' @return numeric vector of the same length as the longest input (standard R
#'   recycling applies).
#' @export
#' @importFrom cli cli_abort
#' @example R/examples/Example_sinusoidal_interval_mean.R
sinusoidal_interval_mean <- function(A = NULL, t1 = NULL, t2 = NULL, phi = NULL) {
  if (is.null(A))   cli::cli_abort("{.arg A} must be provided.")
  if (is.null(t1))  cli::cli_abort("{.arg t1} must be provided.")
  if (is.null(t2))  cli::cli_abort("{.arg t2} must be provided.")
  if (is.null(phi)) cli::cli_abort("{.arg phi} must be provided.")

  result <- A * (12 / pi / (t2 - t1)) * (sin(pi / 12 * t2 + phi) - sin(pi / 12 * t1 + phi))
  return(result)
}
