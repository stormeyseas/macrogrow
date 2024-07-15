#' Relative nutrient quotient
#'
#' @param Q_int Non-dimensionalised internal nutrient quotient
#' @param spec_params A vector of named numbers. Must include:
#'    * \eqn{Q_{min}}, the minimum internal nutrient quotient
#'    * \eqn{Q_{max}}, the maximum internal nutrient quotient
#'
#' @return the relative (0-1) internal nutrient quotient
#' @export
#'
Q_rel <- function(Q_int, spec_params) {
  if (is.na(spec_params['Q_min'])) {abort_missing_parameter(param = "Q_min", place = "spec_params")}
  if (is.na(spec_params['Q_max'])) {abort_missing_parameter(param = "Q_max", place = "spec_params")}
  if (unname(Q_int) < spec_params['Q_min']) {rlang::abort("Q too low! Q_int cannot be < Q_min!", class = "error_bad_parameter")}
  if (unname(Q_int) > spec_params['Q_max']) {rlang::abort("Q too high! Q_int cannot be > Q_max!", class = "error_bad_parameter")}}
  
  Q_rel <- (spec_params['Q_max'] - Q_int)/(spec_params['Q_max'] - spec_params['Q_min'])
  return(unname(Q_rel))
}
