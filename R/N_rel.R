#' Relative internal nitrogen quotient
#'
#' @param N_int the internal nitrogen concentration (percentage, as decimal)
#' @param spec_params a vector of named numbers. Must include:
#'  * `N_min`, the minimum internal nitrogen concentration
#'  * `N_max`, the maximum internal nitrogen concentration
#'  
#'
#' @return relative internal nitrogen quotient
#' @export
#' @seealso [N_int(), Q_rel(), Q_int()]
#'
N_rel <- function(N_int, spec_params) {
  if (is.na(spec_params['N_max'])) {abort_missing_parameter(param = "N_max", place = "spec_params")}
  if (is.na(spec_params['N_min'])) {abort_missing_parameter(param = "N_min", place = "spec_params")}
  
  N_rel <- 1 - ((spec_params["N_max"] - N_int)/(spec_params["N_max"] - spec_params["N_min"]))
  return(unname(N_rel))
}
