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
#'    * \eqn{Q_{min}}, the minimum internal nutrient quotient
#'
#' @return the non-dimensionalised internal nutrient quotient
#'
#' @examples examples
Q <- function(Nf, Ns, spec_params) {
  Q <- spec_params['Q_min'] * (1 + N_s/N_f)
  return(Q)
}

#' Relative nutrient quotient
#'
#' @param Q Non-dimentionalised internal nutrient quotient
#' @param spec_params A vector of named numbers. Must include:
#'    * \eqn{Q_{min}}, the minimum internal nutrient quotient
#'    * \eqn{Q_{max}}, the maximum internal nutrient quotient
#'    * \eqn{K_{c}}, the half-saturation constant for internal nutrient reserves
#'
#' @return the relative (0-1) internal nutrient quotient
#' @export
#'
#' @examples examples
QQ <- function(Q, spec_params) {
  QQ <- (spec_params['Q_max'] - Q)/(spec_params['Q_max'] - spec_params['Q_min'])
  return(QQ)
}

#' Internal nitrogen concentration
#'
#' @param Q,QQ the non-dimensionalised internal nutrient quotient (Q) or relative internal nutrient quotient (QQ). 
#' Only one is needed; if both are supplied only Q is used.
#' @param spec_params a vector of named numbers. Must include:
#'    * \eqn{N_{min}}, the minimum internal nitrogen concentration
#'    * \eqn{N_{max}}, the maximum internal nitrogen concentration
#'    * \eqn{Q_{min}}, the minimum nutrient quotient (if QQ is not supplied)
#'    * \eqn{Q_{max}}, the maximum nutrient quotient (if QQ is not supplied)
#' 
#' @return the internal nitrogen concentration (percentage, as decimal)
#' @export
#'
N_int <- function(Q, QQ, spec_params) {
  if (missing(Q)) {
    N_int <- spec_params['N_max'] - (spec_params['N_max'] - spec_params['N_min']) * QQ
  } else if (missing(QQ)) {
    N_int <- spec_params['N_max'] - (spec_params['N_max'] - spec_params['N_min']) * QQ(Q, spec_params)
  } else {
    warning("Both QQ and Q supplied: calculating internal nitrogen from Q only")
    N_int <- spec_params['N_max'] - (spec_params['N_max'] - spec_params['N_min']) * QQ(Q, spec_params)
  }
  return(N_int)
}

#' Relative internal nitrogen quotient
#'
#' @param N_int the internal nitrogen concentration (percentage, as decimal)
#' @param spec_params a vector of named numbers. Must include:
#'    * \eqn{N_{min}}, the minimum internal nitrogen concentration
#'    * \eqn{N_{max}}, the maximum internal nitrogen concentration
#'
#' @return relative internal nitrogen quotient
#' @export
#'
NN <- function(N_int, spec_params) {
  NN <- (spec_params["N_max"] - N_int)/(spec_params["N_max"] - spec_params["N_min"])
  return(NN)
}

#' Uptake rate (Michaelis-Menton)
#'
#' @param conc external substrate concentration
#' @param V the maximum uptake rate \eqn{V_{max}}
#' @param K the half-saturation constant \eqn{K_{c}}
#'
#' @return the rate of uptake at the specified external concentration
#' @export
#'
#' @examples examples
MM_uptake <- function(conc, V, K) {
  uprate <- (V * conc / (K + conc))
  return(uprate)
}

#' Uptake rate (linear)
#' 
#' @inheritParams MM_uptake
#' @param M the slope of N uptake with increasing substrate concentration 
#' @param C the intercept
#'
#' @return the rate of uptake at the specified external concentration
#' @export
#'
#' @examples examples
lin_uptake <- function(conc, M, C) {
  uprate <- M * conc + C
  return(uprate)
}

#' Macroalgae height
#'
#' @param Nf 
#' @param spec_params A vector of named numbers. Must include the parameters:
#' * \eqn{h_a}, \eqn{h_b} and \eqn{h_c}, parameters governing height change with \eqn{N_f}
#'
#' @return a scalar of macroalgae height (m)
#'
#' @examples examples
algae_height <- function(Nf, spec_params) {
  hm <- (Nf/spec_params['h_a'])^spec_params['h_b'] + spec_params['h_c']
  return(hm)
}

