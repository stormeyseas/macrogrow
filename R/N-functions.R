#' Internal nitrogen concentration
#'
#' @param Q,QQ Non-dimentionalised internal nutrient quotient (Q) or relative nutrient quotient (QQ). 
#' Only one is needed; if both are supplied only Q is used.
#' @param spec_params A vector of named numbers. Must include:
#'    * \eqn{N_{min}}, the minimum internal nitrogen concentration
#'    * \eqn{N_{max}}, the maximum internal nitrogen concentration
#'    * \eqn{Q_{min}}, the minimum nutrient quotient (if QQ is not supplied)
#'    * \eqn{Q_{max}}, the maximum nutrient quotient (if QQ is not supplied)
#' 
#' @return
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

#' Relative internal nitrogen concentration
#'
#' @param N_int the internal nitrogen concentration
#' @param spec_params A vector of named numbers. Must include:
#'    * \eqn{N_{min}}, the minimum internal nitrogen concentration
#'    * \eqn{N_{max}}, the maximum internal nitrogen concentration
#'
#' @return
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
#' @return
#' @export
#'
#' @examples
MM_uptake <- function(conc, V, K) {
  uprate <- (V * conc / (K + conc))
  return(uprate)
}

#' Uptake rate (linear)
#' 
#' @inheritParams MM_uptake
#' @param M 
#' @param C 
#'
#' @return
#' @export
#'
#' @examples
lin_uptake <- function(conc, M, C) {
  uprate <- M * conc + C
  return(uprate)
}

#' Convert mg N m\eqn{^{-3}} d\eqn{^{-1}} to \eqn{\mu} mol N hr\eqn{^{-1}}
#'
#' @param N_mg_m3 Nitrogen (or any compound containing a single N atom) concentration in mg N m^{-3} d^{-1}
#'
#' @return Nitrogen concentration in \mu mol N hr^{-1}
#' @export
#'
#' @examples
#' 
N_mgd_umolh <- function(N_mg_m3){
  umol_h = N_mg_m3 * 10^3 * # from milli to micro
    14.0067^-1 * # from g N to mol N
    24^-1 # from days to hours
  return(umol_h)
}

#' Convert mg N m^{-3} to $\mu$mol N L^{-1}
#'
#' @param mg_m3 Nitrogen (or any compound containing a single N atom) concentration in mg N m^{-3}
#'
#' @return Nitrogen concentration in $\mu$mol N L^{-1}
#' @export
#'
#' @examples
#' 
N_mgm3_umolL <- function(mg_m3){
  umol_L = mg_m3 / 14.0067
  return(umol_L)
}

#' Convert $\mu$mol N L^{-1} to mg N m^{-3}
#'
#' @param umol_L Nitrogen (or any compound containing a single N atom) concentration in $\mu$mol N L^{-1}
#'
#' @return Nitrogen concentration in mg N m$^{-3}$
#' @export
#'
#' @examples
#' 
N_umolL_mgm3 <- function(umol_L){
  mg_m3 = umol_L * 14.0067
  return(mg_m3)
}

