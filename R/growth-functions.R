#' Temperature limitation on growth
#' 
#' Given species parameters returns the relative limitation on growth rate according to a CTMI curve.
#'
#' @param TT temperature to evaluate
#' @param spec_params
#' @return A dataframe with relative temperature limitation (between 0 and 1) at each timestep.
#' @export
#'
#' @examples
#' 
T_lim <- function(TT, spec_params){
  
  if (spec_params['T_opt'] < spec_params['T_min']) {stop("Error: minimum temperature is higher than optimum temperature")}
  if (spec_params['T_opt'] > spec_params['T_max']) {stop("Error: maximum temperature is lower than optimum temperature")} 
  if (spec_params['T_opt']-spec_params['T_min'] <= spec_params['T_max']-spec_params['T_opt']) {stop("Species CTMI function not valid! Must satisfy T_opt-T_min > T_max-T_opt")}
  
  if (is.vector(TT) && is.atomic(TT)) {
    Tlim <- ((TT - spec_params['T_max'])*(TT - spec_params['T_min'])^2)/((spec_params['T_opt'] - spec_params['T_min'])*((spec_params['T_opt'] - spec_params['T_min'])*(TT - spec_params['T_opt']) - (spec_params['T_opt'] - spec_params['T_max'])*(spec_params['T_opt'] + spec_params['T_min'] - 2*TT)))
    Tlim[TT >= spec_params['T_max']] <- 0
    Tlim[TT <= spec_params['T_min']] <- 0
  } else if (is.numeric(TT) | is.integer(TT)) {
    if (TT >= spec_params['T_max']) {Tlim <- 0
    } else if (TT <= spec_params['T_min']) {Tlim <- 0
    } else {
      Tlim <- ((TT - spec_params['T_max'])*(TT - spec_params['T_min'])^2)/((spec_params['T_opt'] - spec_params['T_min'])*((spec_params['T_opt'] - spec_params['T_min'])*(TT - spec_params['T_opt']) - (spec_params['T_opt'] - spec_params['T_max'])*(spec_params['T_opt'] + spec_params['T_min'] - 2*TT)))
    }
  }
  
  return(Tlim)
}

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
#' 
#'
#' @examples
u_c <- function(U0, 
                macro_state = c(biomass, hm), 
                SA_WW = 0.5 * (0.0306/2), site_params, 
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
#' @return
#' @export
#' 
#' @inheritParams u_c
#'
#' @examples
u_b <- function(U0, macro_state = c(biomass, hm), SA_WW = 0.5 * (0.0306/2), site_params, constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)){
  uc <- uc(U0, macro_state, SA_WW, site_params, constants)
  Hc <- (site_params['d_top'] + site_params['hc']) / site_params['hz']
  u_b <- (1 - u_c * Hc) / (1 - Hc)
  return(u_b)
}

#' Total drag coefficient
#'
#' @return The total drag coefficient C_t
#' @export
#' 
#' @inheritParams u_c
#' 
C_t <- function(u_c, u_b, macro_state = c(biomass, hm), SA_WW = 0.5 * (0.0306/2), site_params, constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)) {
  D <- SA_WW * min(macro_state['hm']/site_params['hc'], 1) * macro_state['biomass']
  Kd <- 0.5 * site_params['hz'] * D * constants['s'] * U0^(constants['gam'] - 2)
  Hc <- (site_params['d_top'] + site_params['hc']) / site_params['hz']
  C_t <- (Kd * Hc * u_c ^ 2 + constants['Cb'] * u_b^2)
}

#' Macroalgae height
#'
#' @param Nf 
#' @param spec_params A vector of named numbers. Must include the parameters \eqn{h_a}, \eqn{h_b} and \eqn{h_c}
#'
#' @return
#'
#' @examples
algae_height <- function(Nf, spec_params) {
  hm <- (Nf/spec_params['h_a'])^spec_params['h_b'] + spec_params['h_c']
  return(hm)
}

#' Light limitation on growth
#'
#' @param Nf 
#' @param I 
#' @param spec_params A vector of named numbers. Must include:
#'    * \eqn{a_{cs}}
#'    * \eqn{I_o}
#'    * \eqn{h_a}, \eqn{h_b} and \eqn{h_c}
#' @param site_params A vector of named numbers. Must include:
#'    * \eqn{d_{top}}
#'    * \eqn{kW}
#'
#' @return
#' @export
#'
#' @examples
I_lim <- function(Nf, I_top, spec_params, site_params) {
  hm <- algae_height(Nf, spec_params)
  k_ma <-  Nf * spec_params['a_cs'] * max(hm / site_params['d_top'], 1) * 1 / (min(hm, site_params['d_top']))
  
  x <- exp(I_top / spec_params['I_o'])
  K <- site_params['kW'] + k_ma
  
  Ilim <- (exp(1) / (K * site_params['d_top'])) * ((1 / x) ^ (1 / exp(K * site_params['d_top'])) - 1 / x)

  return(Ilim)
}

