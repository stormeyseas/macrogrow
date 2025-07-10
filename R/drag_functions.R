#' Relative water attenuation within canopy
#' 
#' #' @description
#' A short description...
#'
#' @param U0 incoming incident water velocity (m/s)
#' @param macro_state vector of named numbers. Must include:
#'  * `biomass`, macroalgae wet weight (g)
#'  * `hm`, algae height (m)
#' @param site_params vector of named numbers. Must include:
#'  * `hz`, total water depth (m)
#'  * `hc`, vertical water column occupied by the canopy (m)
#'  * `d_top`, depth of the top of the canopy beneath the water surface (m)
#' @param spec_params vector of named numbers. Must include:
#'  * `SA_WW`, conversion of wet weight to surface area
#' @param constants vector of named numbers defining extra constants for the attenuation submodel. Must include all constants, which have the following default values:
#'  * `s` = 0.0045
#'  * `gam` = 1.13
#'  * `a2` = 0.2^2
#'  * `Cb` = 0.0025
#'
#' @return the relative water attenuation coefficient (u_c)
#' @export
#' @seealso [algae_height(), u_b(), C_t()]
#' 
u_c <- function(U0, macro_state, site_params, spec_params, 
  constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)){
  
  if (missing(spec_params) | is.na(spec_params["SA_WW"])) {
    SA_WW <- 0.5 * (0.0306/2) # default is based on Macrocystis pyrifera
  } else {
    SA_WW <- spec_params["SA_WW"]
  }
  
  D <- SA_WW * min(macro_state['hm']/abs(site_params['hc']), 1) * macro_state['biomass']
  Kd <- 0.5 * abs(site_params['hz']) * D * constants['s'] * U0^(constants['gam'] - 2)
  Hc <- (abs(site_params['d_top']) + abs(site_params['hc'])) / abs(site_params['hz'])
  
  drag_test <- unname(suppressWarnings(
    sqrt(Kd * (1 - Hc) * Hc * (constants['Cb'] * Hc + constants['a2']) - constants['a2'] * constants['Cb'] * Hc)
    ))
    
  u_c <- if (is.na(drag_test)) {1} else {
    (-constants['a2'] - constants['Cb'] * Hc ^ 2 + (1 - Hc) * drag_test) / (Kd * Hc * (1 - Hc) ^ 3 - constants['a2'] - constants['Cb'] * Hc ^ 3)
  }
  return(unname(u_c))
}

#' Relative water attenuation beneath canopy
#'
#' @inheritParams u_c
#'
#' @return a scalar of relative water attenuation beneath canopy
#' @export
#' 
#' @examples examples
u_b <- function(U0, macro_state, SA_WW = 0.5 * (0.0306/2), site_params, 
  constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)){
  uc <- uc(U0, macro_state, SA_WW, site_params, constants)
  Hc <- (site_params['d_top'] + site_params['hc']) / site_params['hz']
  u_b <- (1 - u_c * Hc) / (1 - Hc)
  return(unname(u_b))
}

#' Total drag coefficient
#'
#' @inheritParams u_c
#' 
#' @return The total drag coefficient C_t
#' 
C_t <- function(U0, macro_state, site_params, spec_params, 
  constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)) {
  
  if (missing(spec_params) | is.na(spec_params["SA_WW"])) {
    SA_WW <- 0.5 * (0.0306/2) # default is based on Macrocystis pyrifera
  } else {
    SA_WW <- spec_params["SA_WW"]
  }
  D <- SA_WW * min(macro_state['hm']/abs(site_params['hc']), 1) * macro_state['biomass']
  Kd <- 0.5 * abs(site_params['hz']) * D * constants['s'] * U0^(constants['gam'] - 2)
  Hc <- (abs(site_params['d_top']) + abs(site_params['hc'])) / abs(site_params['hz'])
  u_c <- u_c(U0, macro_state, site_params, spec_params, constants)

  C_t <- (Kd * Hc * u_c ^ 2 + constants['Cb'] * u_b^2)
  return(unname(C_t))
}
