#' Temperature at site
#' 
#' @description
#' A short description... 
#'
#' @param tspan A vector of timesteps
#' @param a The mean temperature across timesteps at the site
#' @param b The temporal phase shift (the timestep where the temperature is at maximum)
#' @param c The amplitude multiplier for the curve
#' @param period The total number of timesteps
#'
#' @return A vector of temperatures for the given site
#' 
#' @details
#' A simple temperature curve in the form: 
#' \deqn{\begin{array}[ccc]
#' T = T_a + \frac{T_c}{period}  * \sin\left(\frac{2\pi(t+T_b) + \pi}{2}\right)
#' \end{array}} 
#'
#' @examples examples
#' # Generate temperature fluctuations in a day
#' day <- seq(1, 24*60, 5)
#' temp <- temp_curve(tspan = day, 18.5, 14*60, 2.5)
#' plot(day, temp)
#' 
#' # Generate temperature fluctuations for part of the year
#' yearpart <- seq(90, 185, 1)
#' temp <- temp_curve(tspan = yearpart, 18.5, 60, 2.5, period = 365)
#' plot(yearpart, temp)
#' 
curve_Temperature <- function(tspan = seq(1,365,1), a, b, c, period) {
  if (missing(period)) {
    period <- max(tspan)
    rlang::inform(message = glue::glue("Period defaulting to maximum tstep = {period}.", class = "implicit_defaults"))
  }
  temp <- a + b * sin((2 * pi * (tspan + c) + pi / 2) / period)
  return(unname(temp))
}
