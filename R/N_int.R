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
  if (is.na(spec_params['N_max'])) {abort_missing_parameter(param = "N_max", place = "spec_params")}
  if (is.na(spec_params['N_min'])) {abort_missing_parameter(param = "N_min", place = "spec_params")}
  
  if (missing(Q)) {
    N_int <- spec_params['N_max'] - (spec_params['N_max'] - spec_params['N_min']) * QQ
  } else if (missing(QQ)) {
    if (is.na(spec_params['Q_min'])) {abort_missing_parameter(param = "Q_min", place = "spec_params")}
    if (is.na(spec_params['Q_max'])) {abort_missing_parameter(param = "Q_max", place = "spec_params")}
    
    N_int <- spec_params['N_max'] - (spec_params['N_max'] - spec_params['N_min']) * QQ(Q, spec_params)
  } else {
    rlang::inform(message = "Both QQ and Q supplied: calculating from Q only", class = redundant_parameters)
    N_int <- spec_params['N_max'] - (spec_params['N_max'] - spec_params['N_min']) * QQ(Q, spec_params)
  }
  return(N_int)
}
