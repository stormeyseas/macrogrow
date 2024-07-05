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
  if (is.na(spec_params['N_max'])) {abort_missing_parameter(param = "N_max", place = "spec_params")}
  if (is.na(spec_params['N_min'])) {abort_missing_parameter(param = "N_min", place = "spec_params")}
  
  NN <- (spec_params["N_max"] - N_int)/(spec_params["N_max"] - spec_params["N_min"])
  return(NN)
}
