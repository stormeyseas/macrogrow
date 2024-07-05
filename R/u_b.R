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
