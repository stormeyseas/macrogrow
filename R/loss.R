#' Loss through fragmentation
#' 
#' @description
#' The macroalgae fragmentation rate (\eqn{D}) is calculated as:
#' \deqn{D = D_{ve} \times U_0 + D_{tu} + D_{m}} 
#' where \eqn{D_{ve}} is the linear coefficient of loss with laminar velocity (\eqn{U_0}), 
#' \eqn{D_{tu}} is the loss rate at the specified level of turbulence (static, low, medium, or high), and
#' \eqn{D_{m}} is the base loss rate.
#'
#' @inheritParams u_c 
#' @param turbulence optional level of turbulence. Options are "none"/"static", "low", "medium"/"mid", and "high".
#' @param spec_params a vector of named numbers. All default to 0 if not supplied. Can include: 
#'  * `D_m`, a constant (base) loss rate
#'  * `D_ve`, the linear coefficient of loss with laminar velocity
#'  * `D_st`, the loss rate in static water (turbulence = 0, "none" or "static")
#'  * `D_lo`, the loss rate in turbulent water (turbulence = 1 or "low")
#'  * `D_mi`, the loss rate in turbulent water (turbulence = 2 or "medium")
#'  * `D_hi`, the loss rate in turbulent water (turbulence = 3 or "high")
#'
#' @details
#' Note that parameter `U0` has no default. 
#' If one is not supplied, loss due to velocity will default to 0 regardless of the value of `D_ve` supplied. 
#' 
#' @return a scalar for macroalgae loss (d-1)
#' @export
#'
#' @examples examples
#' 
loss <- function(U0, turbulence = 0, spec_params) {

  D_m <- ifelse(is.na(spec_params['D_m']), 0, spec_params['D_m'])  
  D_ve <- ifelse(is.na(spec_params['D_ve']), 0, spec_params['D_ve'])

  D_turbulence <- if (turbulence == "none" | turbulence == "static") {
    ifelse(is.na(spec_params['D_st']), 0, spec_params['D_st'])
  } else if (turbulence == "low") {
    ifelse(is.na(spec_params['D_lo']), 0, spec_params['D_lo'])
  } else if (turbulence == "medium" | turbulence == "mid") {
    ifelse(is.na(spec_params['D_mi']), 0, spec_params['D_mi'])
  } else if (turbulence == "high") {
    ifelse(is.na(spec_params['D_hi']), 0, spec_params['D_hi'])
  } else {
    NA
  }

  # Actual loss calculation
  D_m <- U0 * D_ve + D_turbulence + D_m
  
  return(unname(D_m))
}
