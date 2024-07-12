#' Nitrogen at site
#' 
#' @description
#' A short description... 
#'
#' @param tspan A vector of timesteps
#' @param a The mean NX concentration across timesteps at the site
#' @param b The temporal phase shift (the timestep where concentration is at maximum)
#' @param c The amplitude multiplier for the curve
#' @param period The total number of timesteps
#'
#' @return A vector of concentrations for the given site
#' 
#' @details
#' A simple sine curve in the form: 
#' \deqn{\begin{array}[ccc]
#' N = N_a + \frac{N_c}{period} \sin\left(\frac{2\pi(t+N_b) + \pi}{2}\right)
#' \end{array}} 
#'
#' @examples examples
#' 
N_curve <- function(tspan = seq(1,365,1), a, b, c, period) {
  if (missing(period)) {
    period <- max(tspan)
    rlang::inform(message = glue::glue("Period defaulting to maximum tstep = {period}.", class = "implicit_defaults"))
  }
  N <- a + b * sin((2 * pi * (tspan - c) + pi / 2) / period)
  return(unname(N))
}
