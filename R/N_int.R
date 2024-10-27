#' Internal nitrogen concentration
#'
#' @param Q_int, Q_rel the non-dimensionalised internal nutrient quotient (\eqn{Q}) or relative internal nutrient quotient (\eqn{Q_{rel}}). 
#' Only one is needed; if both are supplied only \eqn{Q_{rel}} is used.
#' @param spec_params a vector of named numbers. Must include:
#'  * `N_min`, the minimum internal nitrogen concentration
#'  * `N_max`, the maximum internal nitrogen concentration
#'  * `Q_min`, the minimum nutrient quotient (if `Q_rel` is not supplied)
#'  * `Q_max`, the maximum nutrient quotient (if `Q_rel` is not supplied)
#' 
#' @return the internal nitrogen concentration (percentage, as decimal)
#' @export
#' @seealso [Q_int(), N_rel(), Q_rel()]
#'
N_int <- function(Q_int = NA, Q_rel = NA, spec_params) {
  if (is.na(spec_params['N_max'])) {abort_missing_parameter(param = "N_max", place = "spec_params")}
  if (is.na(spec_params['N_min'])) {abort_missing_parameter(param = "N_min", place = "spec_params")}
  
  if (!is.na(Q_rel)) {
    N_int <- spec_params['N_max'] - (spec_params['N_max'] - spec_params['N_min']) * Q_rel
  } else if (!is.na(Q_int)) {
    if (is.na(spec_params['Q_min'])) {abort_missing_parameter(param = "Q_min", place = "spec_params if Q_rel is not supplied")}
    if (is.na(spec_params['Q_max'])) {abort_missing_parameter(param = "Q_max", place = "spec_params if Q_rel is not supplied")}
    N_int <- spec_params['N_max'] - (spec_params['N_max'] - spec_params['N_min']) * Q_rel(Q = Q_int, spec_params = spec_params)
  }
  return(unname(N_int))
}
