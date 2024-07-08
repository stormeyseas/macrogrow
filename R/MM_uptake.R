#' Uptake rate (Michaelis-Menton)
#'
#' @param conc external substrate concentration
#' @param V the maximum uptake rate \eqn{V_{max}}
#' @param K the half-saturation constant \eqn{K_{c}}
#'
#' @return the rate of uptake at the specified external concentration
#' @export
#'
MM_uptake <- function(conc, V, K) {
  if (missing(V) & missing(K)) {abort_missing_parameter(param = "V and K", place = "spec_params. Did you mean to use lin_uptake() instead?")}
  if (missing(V)) {abort_missing_parameter(param = "V", place = "spec_params and passed to function as 'V'")}
  if (missing(K)) {abort_missing_parameter(param = "K", place = "spec_params and passed to function as 'K'")}
  
  uprate <- (V * conc / (K + conc))
  return(unname(uprate))
}
