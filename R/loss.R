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
  
  # If loss with velocity is missing...
  if (is.na(spec_params['D_ve'])) {
    if (missing(U0) | is.na(U0)) {
      spec_params['D_ve'] <- 0
    } else {
      spec_params['D_ve'] <- 0
      rlang::inform("Velocity unused - no velocity loss parameter provided")
    }
  } else {
    if (missing(U0) | is.na(U0)) {
      spec_params['D_ve'] <- 0
      rlang::inform("Warning: velocity loss parameter provided but U0 is missing")
    }
  }

  # if (is.na(turbulence)) {
    D_turbulence <- 0
  #   if (!is.na(spec_params['D_st'])) {rlang::inform("Warning: turbulence loss parameter(s) provided but turbulence level = NA.")}
  #   
  # } else {
  #   if (turbulence == 0 | turbulence == "none" | turbulence == "static") {
  #     if (!is.na(spec_params['D_st'])) {
  #       D_turbulence <- spec_params['D_st']
  #     } else {
  #       rlang::abort(glue::glue("Turbulence is `{turbulence}` but 'D_st' parameter not provided"), class = "error_bad_parameter")
  #     }
  #   } else if (turbulence == 1 | turbulence == "low") {
  #     if (!is.na(spec_params['D_low'])) {
  #       D_turbulence <- spec_params['D_low']
  #     } else {
  #       rlang::abort(glue::glue("Turbulence is `{turbulence}` but 'D_low' parameter not provided"), class = "error_bad_parameter")
  #     }
  #   } else if (turbulence == 2 | turbulence == "medium" | turbulence == "mid") {
  #     if (!is.na(spec_params['D_mi'])) {
  #       D_turbulence <- spec_params['D_mi']
  #     } else {
  #       rlang::abort(glue::glue("Turbulence is `{turbulence}` but 'D_mi' parameter not provided"), class = "error_bad_parameter")
  #     }
  #   } else if (turbulence == 3 | turbulence == "high") {
  #     if (!is.na(spec_params['D_hi'])) {
  #       D_turbulence <- spec_params['D_hi']
  #     } else {
  #       rlang::abort(glue::glue("Turbulence is `{turbulence}` but 'D_hi' parameter not provided"), class = "error_bad_parameter")
  #     }
  #   } else {
  #     rlang::inform("Your turbulence level is not recognised in FORT KICKASS")
  #   }
  # }
  
  if (is.na(spec_params['D_m'])) {
    spec_params['D_m'] <- 0
  }

  # Actual loss calculation
  D_m <- U0 * spec_params['D_ve'] + D_turbulence + spec_params['D_m']
  
  return(unname(D_m))
}
