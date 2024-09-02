#' Loss through fragmentation
#' 
#' @description
#' The macroalgae fragmentation rate is calculated as:
#' \begin{equation}
#' D_m = D_{velocity} \times U_0 + D_{turbulence} + D_{base}
#' \end{equation} 
#' where $D_{velocity}$ is the linear coefficient of loss with laminar velocity, 
#' $D_{turbulence}$ is the loss rate at the specified level of turbulence (static, low, medium, or high), and
#' $D_{base}$ is the base loss rate.
#'
#' @inheritParams u_c
#' @param turbulence optional level of turbulence. Options are "none" (or "static"), "low", "medium" (or "mid"), and "high".
#' @param spec_params a vector of named numbers. All default to 0 if not supplied. Can include:
#'  * `D_m`, a constant (base) loss rate
#'  * `D_ve`, the linear coefficient of loss with laminar velocity
#'  * `D_st`, the loss rate in static water (turbulence = "none" or "static")
#'  * `D_lo`, the loss rate in turbulent water (turbulence = "low")
#'  * `D_mi`, the loss rate in turbulent water (turbulence = "medium")
#'  * `D_hi`, the loss rate in turbulent water (turbulence = "high")
#'
#' @return a scalar for macroalgae loss (% d-1)
#' @export
#'
#' @examples examples
#' 
loss <- function(U0 = 0, turbulence = NA, spec_params) {
  
  if (!is.na(spec_params['D_m'])) {
    D_base <- spec_params['D_m']
  } else {
    D_base <- 0
  }
    
  if (!is.na(spec_params['D_ve'])) {
    D_velocity <- U0 * spec_params['D_ve']
  } else {
    # No loss with velocity
    D_velocity <- 0
  }
    
  if (!is.na(turbulence)) {
    if (turbulence == "none" | turbulence == "static" & !is.na(spec_params['D_st'])) {
      D_turbulence <- spec_params['D_st']
    } else if (turbulence == "low" & !is.na(spec_params['D_lo'])) {
      D_turbulence <- spec_params['D_low']
    } else if (turbulence == "medium" | turbulence == "mid" & !is.na(spec_params['D_mi'])) {
      D_turbulence <- spec_params['D_mi']
    } else if (turbulence == "high" & !is.na(spec_params['D_hi'])) {
      D_turbulence <- spec_params['D_hi']
    } else {
      rlang::abort(glue::glue("Turbulence is `{turbulence}` but corresponding loss parameter not provided"), class = "error_bad_parameter")
      }
  } else {
    D_turbulence <- 0
    if (!is.na(spec_params['D_st']) | !is.na(spec_params['D_st']) | !is.na(spec_params['D_st']) | !is.na(spec_params['D_st'])) {
      rlang::inform("Warning: turbulence loss parameter(s) provided but turbulence level = NA.")
    }
  }

  D_m <- D_velocity + D_turbulence + D_base
  
  return(unname(D_m))
}
