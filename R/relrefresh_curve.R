#' Relative refresh rate at site
#' 
#' @description
#' A short description... 
#'
#' @param tspan A vector of timesteps
#' @param a The mean irradience across timesteps at the site
#' @param b The temporal phase shift (the timestep where irradience is at maximum)
#' @param c The amplitude multiplier for the curve
#' @param period The total number of timesteps
#'
#' @return An irradience vector for the given site
#' 
#' @details
#' A simple sine curve in the form: 
#' \deqn{\begin{array}[ccc]
#' U = U_a + \frac{U_c}{period} \sin\left(\frac{2\pi(t+U_b) + \pi}{2}\right)
#' \end{array}} 
#'
#' @examples examples
#' 
relrefresh_curve <- function(tspan = seq(1,365,1), a, b, c, period) {
  if (missing(period)) {
    period <- max(tspan)
    rlang::inform(message = glue::glue("Period defaulting to maximum tstep = {period}.", class = "implicit_defaults"))
  }
  U <- a + b * sin((2 * pi * (tspan - c) + pi / 2) / period)
  return(unname(U))
}
