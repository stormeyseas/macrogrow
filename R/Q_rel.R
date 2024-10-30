#' Relative nutrient quotient
#'
#' @param Q_int Non-dimensionalised internal nutrient quotient
#' @param spec_params A vector of named numbers. Must include:
#'  * `Q_min`, the minimum internal nutrient quotient
#'  * `Q_max`, the maximum internal nutrient quotient
#'
#' @return the relative (0-1) internal nutrient quotient
#' @export
#'
Q_rel <- function(Nf = NULL, Ns = NULL, Q_int = NULL, N_int = NULL, Q_rel = NULL, N_rel = NULL, spec_params) {
  # if (is.na(spec_params['Q_min'])) {abort_missing_parameter(param = "Q_min", place = "spec_params")}
  # if (is.na(spec_params['Q_max'])) {abort_missing_parameter(param = "Q_max", place = "spec_params")}
  
  if (!is.null(N_int)) {N_rel <- N_rel(N_int = N_int, spec_params = spec_params)}
  if (!is.null(N_rel)) {
    Q_rel <- N_rel
  }
  
  if (!is.null(Nf) & !is.null(Ns)) {Q_int <- Q_int(Nf = Nf, Ns = Ns, spec_params = spec_params)}
  if (!is.null(Q_int)) {
    Q_rel <- 1 - (spec_params['Q_max'] - Q_int)/(spec_params['Q_max'] - spec_params['Q_min'])
  }
  
  return(unname(Q_rel))
}

  # if (unname(Q_int) < spec_params['Q_min']) {rlang::abort("Q too low! Q_int cannot be < Q_min!", class = "error_bad_parameter")}
  # if (unname(Q_int) > spec_params['Q_max']) {rlang::abort("Q too high! Q_int cannot be > Q_max!", class = "error_bad_parameter")}
  
  