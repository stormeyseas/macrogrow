#' Relative water attenuation within canopy
#' 
#' #' @description
#' A short description...
#'
#' @param U0 incoming incident water velocity (m/s)
#' @param macro_state vector of named numbers in the form c(biomass, hm) defining macroalgae wet weight (g) and height (m)
#' @param SA_WW conversion of wet weight to surface area (default is 0.5*0.5*0.0306 for \textit{Macrocystis pyrifera})
#' @param site_params vector of named numbers in the form c(hz, hc, site_params['d_top']) defining total water depth (m), vertical water column occupied by the canopy (m), and depth of the top of the canopy beneath the water surface (m)
#' @param constants vector of named numbers in the form c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025) defining extra constants for the attenuation submodel
#'
#' @return the relative water attenuation coefficient (u_c)
#' @export
#' 
u_c <- function(U0, 
                macro_state = c(biomass, hm), 
                SA_WW = 0.5 * (0.0306/2), 
                site_params, 
                constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)){
  
  D <- SA_WW * min(macro_state['hm']/site_params['hc'], 1) * macro_state['biomass']
  Kd <- 0.5 * site_params['hz'] * D * constants['s'] * U0^(constants['gam'] - 2)
  Hc <- (site_params['d_top'] + site_params['hc']) / site_params['hz']
  
  drag_test <- sqrt(Kd * (1 - Hc) * Hc * (constants['Cb'] * Hc + constants['a2']) - constants['a2'] * constants['Cb'] * Hc)
  if (is.na(drag_test)) {
    u_c <- 1
  } else {
    u_c <- (-constants['a2'] - constants['Cb'] * Hc ^ 2 + (1 - Hc) * drag_test) / (Kd * Hc * (1 - Hc) ^ 3 - constants['a2'] - constants['Cb'] * Hc ^ 3)
  }
  return(u_c)
}

#' Relative water attenuation beneath canopy
#'
#' @inheritParams u_c
#'
#' @return a scalar of relative water attenuation beneath canopy
#' @export
#' 
#' @examples examples
u_b <- function(U0, macro_state = c(biomass, hm), SA_WW = 0.5 * (0.0306/2), site_params, constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)){
  uc <- uc(U0, macro_state, SA_WW, site_params, constants)
  Hc <- (site_params['d_top'] + site_params['hc']) / site_params['hz']
  u_b <- (1 - u_c * Hc) / (1 - Hc)
  return(u_b)
}

#' Total drag coefficient
#'
#' @inheritParams u_c
#' 
#' @return The total drag coefficient C_t
#' @export
#' 
C_t <- function(u_c, u_b, macro_state = c(biomass, hm), SA_WW = 0.5 * (0.0306/2), site_params, constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)) {
  D <- SA_WW * min(macro_state['hm']/site_params['hc'], 1) * macro_state['biomass']
  Kd <- 0.5 * site_params['hz'] * D * constants['s'] * U0^(constants['gam'] - 2)
  Hc <- (site_params['d_top'] + site_params['hc']) / site_params['hz']
  C_t <- (Kd * Hc * u_c ^ 2 + constants['Cb'] * u_b^2)
}

