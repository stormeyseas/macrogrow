#' Temperature limitation on growth
#' 
#' Given species parameters returns the relative limitation on growth rate according to a CTMI curve.
#'
#' @param TT temperature to evaluate
#' @param spec_params a vector of named numbers. Must include:
#'    * \eqn{T_{opt}} the optimum temperature for macroalgae growth
#'    * \eqn{T_{min}} the minimum temperature for macroalgae growth (when \eqn{T < T_{min}}, growth = 0)
#'    * \eqn{T_{max}} the maximum temperature for macroalgae growth (when \eqn{T > T_{max}}, growth = 0)
#'
#' @return a scalar of relative temperature limitation on growth (between 0 and 1)
#' @export
#'
#' @examples examples examples
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

#' Light limitation on growth
#' 
#' @description
#' Calculates the relative limitation on growth rate due to light availability, via:
#' \deqn{\begin{array}[ccc] 
#' I_{lim} &=& \frac{e}{k d_{top}} \times \Biggl[e^{-\frac{I_z e^{-k d_{top}}}{I_o}} - e^{-\frac{I_z}{I_o}} \Biggr]
#' \end{array}}
#' where \eqn{$I_{z}=I e^{-k_W \cdot d_{top}}$} is the irradiance at the cultivation depth,
#' and \eqn{k=k_{m}+kW} is the total attenuation coefficient.
#' \eqn{k_{m}} is the additional attenuation coefficient from macroalgae biomass, calculated as:
#' \deqn{\begin{array}[ccc] 
#' k_{m} &=& a_{cs} \times N_f \times \text{max} \Biggl( \frac{h_{m}}{d_{top}}, 1 \Biggr) \times \frac{1}{\text{min}(h_{m}, d_{top})}
#' \end{array}}
#' where \eqn{h_{m}} is the macroalgae height.
#' 
#' @inheritParams Q
#' @param I the surface irradiance, PAR (\eqn{\mu}mol photons m\eqn{^{-2}} s\eqn{^{-1}})
#' @param spec_params a vector of named numbers. Must include:
#'    * \eqn{a_{cs}}, the carbon-specific self-shading constant
#'    * \eqn{I_o}, the light saturation parameter
#'    * \eqn{h_a}, \eqn{h_b} and \eqn{h_c}, parameters governing height change with \eqn{N_f}
#' @param site_params A vector of named numbers. Must include:
#'    * \eqn{d_{top}} the below-surface  depth (m) of the top of the macroalgae culture
#'    * \eqn{kW} the light-attenuation coefficient of open water
#'
#' @return a scalar of relative light limitation on growth (between 0 and 1)
#' @export
#'
#' @examples examples
#' @seealso [algae_height()]
I_lim <- function(Nf, I, spec_params, site_params) {
  hm <- algae_height(Nf, spec_params)
  k_ma <-  Nf * spec_params['a_cs'] * max(hm / site_params['d_top'], 1) * 1 / (min(hm, site_params['d_top']))
  
  I_top <- # FINISH THIS
  x <- 
  K <- site_params['kW'] + k_ma
  
  Ilim <- (exp(1) / (K * site_params['d_top'])) * ((1 / exp(I_top / spec_params['I_o'])) ^ (1 / exp(K * site_params['d_top'])) - 1 / exp(I_top / spec_params['I_o']))
  
  return(Ilim)
}

#' Q limitation on growth
#'
#' @description Calculates the internal nutrient quotient \eqn{Q} and its relative effect on growth rate \eqn{Q_{lim}} via:
#' \deqn{\begin{array}[ccc] 
#' Q &=& Q_{min} \left(1 + \frac{N_s}{N_f}\right)
#' Q_{lim} &=& \frac{Q - Q_{min}}{Q - K_c}
#' \end{array}}
#' 
#' @inheritParams Q
#' @inheritParams QQ
#' 
#' @return a scalar of relative limitation from internal nutrient reserves on growth (between 0 and 1)
#' 
Q_lim <- function(Nf, Ns, spec_params) {
  Q <- spec_params['Q_min'] * (1 + Ns/Nf)
  if (Q < spec_params['Q_min']) {Q <- spec_params['Q_min']} 
  if (Q > spec_params['Q_max']) {Q <- spec_params['Q_max']}
  
  Q_lim <- (Q - spec_params['Q_min'])/(Q - spec_params['K_c'])
  return(Q_lim)
}

