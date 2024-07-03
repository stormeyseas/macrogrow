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
#' temp <- temp_atsite(tspan = day, 18.5, 14*60, 2.5)
#' plot(day, temp)
#' 
#' # Generate temperature fluctuations for part of the year
#' yearpart <- seq(90, 185, 1)
#' temp <- temp_atsite(tspan = yearpart, 18.5, 60, 2.5, period = 365)
#' plot(yearpart, temp)
#' 
temp_atsite <- function(tspan = seq(1,365,1), a, b, c, period) {
  if (missing(period)) {
    period <- max(tspan)
    cat("period defaulting to maximum tstep =", period, "\n")
  }
  temp <- a + b * sin((2 * pi * (tspan + c) + pi / 2) / period)
  return(temp)
}

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
N_atsite <- function(tspan = seq(1,365,1), a, b, c, period) {
  if (missing(period)) {
    period <- max(tspan)
    cat("period defaulting to maximum tstep =", period, "\n")
  }
  N <- a + b * sin((2 * pi * (tspan - c) + pi / 2) / period)
  return(N)
}

#' Irradience at site
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
#' I = I_a + \frac{I_c}{period} \sin\left(\frac{2\pi(t+I_b) + \pi}{2}\right)
#' \end{array}} 
#'
#' @examples examples
#' 
Irr_atsite <- function(tspan = seq(1,365,1), a, b, c, period) {
  if (missing(period)) {
    period <- max(tspan)
    cat("period defaulting to maximum tstep =", period, "\n")
  }
  I <- a + b * sin((2 * pi * (tspan - c) + pi / 2) / period)
  return(I)
}

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
relrefresh_atsite <- function(tspan = seq(1,365,1), a, b, c, period) {
  if (missing(period)) {
    period <- max(tspan)
    cat("period defaulting to maximum tstep =", period, "\n")
  }
  U <- a + b * sin((2 * pi * (tspan - c) + pi / 2) / period)
  return(U)
}

##' Solar Radiation to PPFD
##'
##' The following function and documentation was copied verbatim from file modules/data.atmosphere/R/metutils.R in https://github.com/PecanProject/pecan/.
##' 
##' There is no easy straight way to convert MJ/m2 to mu mol photons / m2 / s (PAR).
##' Note: 1 Watt = 1J/s
##' The above conversion is based on the following reasoning
##' 0.12 is about how much of the total radiation is expected to ocurr during the hour of maximum insolation (it is a guesstimate)
##' 2.07 is a coefficient which converts from MJ to mol photons (it is approximate and it is taken from ...
##' Campbell and Norman (1998). Introduction to Environmental Biophysics. pg 151 'the energy content of solar radiation in the PAR waveband is 2.35 x 10^5 J/mol'
##' See also the chapter radiation basics (10)
##' Here the input is the total solar radiation so to obtain in the PAR spectrum need to multiply by 0.486
##' This last value 0.486 is based on the approximation that PAR is 0.45-0.50 of the total radiation
##' This means that 1e6 / (2.35e6) * 0.486 = 2.07
##' 1e6 converts from mol to mu mol
##' 1/3600 divides the values in hours to seconds
##'
##' @title MJ to PPFD
##' @author Fernando Miguez
##' @author David LeBauer
##' @param solarMJ MJ per day
##' 
##' @return PPFD umol /m2 / s
##' 
solarMJ2ppfd <- function(solarMJ) {
  ppfd <- (0.12 * solarMJ) * 2.07 * 1e+06 / 3600
  return(ppfd)
} 

