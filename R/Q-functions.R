#' Non-dimentionalised internal nutrient quotient
#' 
#' @description Calculates the internal nutrient quotient \eqn{Q} via:
#' \deqn{\begin{array}[ccc] 
#' Q &=& Q_{min} \left(1 + \frac{N_s}{N_f}\right)
#' \end{array}}
#'
#' @param Nf Fixed nitrogen (mg m\eqn{^{-3}})
#' @param Ns Stored nitrogen (mg m\eqn{^{-3}})
#' @param spec_params A vector of named numbers. Must include:
#'    * \eqn{Q_{min}}, the minimum nutrient quotient
#'
#' @return the non-dimensionalised internal nutrient quotient
#'
#' @examples
Q <- function(Nf, Ns, spec_params) {
  Q <- spec_params['Q_min'] * (1 + N_s/N_f)
  return(Q)
}

#' Relative nutrient quotient
#'
#' @param Q 
#' @param spec_params A vector of named numbers. Must include:
#'    * \eqn{Q_{min}}, the minimum nutrient quotient
#'    * \eqn{Q_{max}}, the minimum nutrient quotient
#'    * \eqn{K_{c}}, the half-saturation constant for internal nutrient reserves
#'
#' @return the relative (0-1) internal nutrient quotient
#' @export
#'
#' @examples
QQ <- function(Q, spec_params) {
  QQ <- (spec_params['Q_max'] - Q)/(spec_params['Q_max'] - spec_params['Q_min'])
  return(QQ)
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
#' @return numeric from 0-1 describing relative growth rate from internal nutrient reserves
#' 
Q_lim <- function(Nf, Ns, spec_params) {
  Q <- spec_params['Q_min'] * (1 + Ns/Nf)
  if (Q < spec_params['Q_min']) {Q <- spec_params['Q_min']} 
  if (Q > spec_params['Q_max']) {Q <- spec_params['Q_max']}
  
  Q_lim <- (Q - spec_params['Q_min'])/(Q - spec_params['K_c'])
  return(Q_lim)
}

