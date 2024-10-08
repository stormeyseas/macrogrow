#' Total drag coefficient
#'
#' @inheritParams u_c
#' 
#' @return The total drag coefficient C_t
#' 
C_t <- function(u_c, u_b, macro_state = c(biomass, hm), SA_WW = 0.5 * (0.0306/2), site_params, constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)) {
  D <- SA_WW * min(macro_state['hm']/site_params['hc'], 1) * macro_state['biomass']
  Kd <- 0.5 * site_params['hz'] * D * constants['s'] * U0^(constants['gam'] - 2)
  Hc <- (site_params['d_top'] + site_params['hc']) / site_params['hz']
  C_t <- (Kd * Hc * u_c ^ 2 + constants['Cb'] * u_b^2)
  return(unname(C_t))
}
