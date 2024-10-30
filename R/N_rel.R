#' Relative internal nitrogen quotient
#'
#' @param N_int the internal nitrogen concentration (percentage, as decimal)
#' @param spec_params a vector of named numbers. Must include:
#'  * `N_min`, the minimum internal nitrogen concentration
#'  * `N_max`, the maximum internal nitrogen concentration
#'
#' @return relative internal nitrogen quotient
#' @export
#' @seealso [N_int(), Q_rel(), Q_int()]
#'
N_rel <- function(Nf = NULL, Ns = NULL, Q_int = NULL, N_int = NULL, Q_rel = NULL, N_rel = NULL, spec_params) {
  
  if (!is.null(Nf) & !is.null(Ns)) {Q_int <- Q_int(Nf = Nf, Ns = Ns, spec_params = spec_params)}
  if (!is.null(Q_int)) {Q_rel <- Q_rel(Q_int = Q_int, spec_params = spec_params)}
  if (!is.null(Q_rel)) {
    N_rel <- Q_rel
  }
  
  if (!is.null(N_int)) {
    N_rel <- 1 - (spec_params['N_max'] - N_int)/(spec_params['N_max'] - spec_params['N_min'])
  }
  
  return(unname(N_rel))
}

# if (is.na(spec_params['N_min'])) {abort_missing_parameter(param = "N_min", place = "spec_params")}
# if (is.na(spec_params['N_max'])) {abort_missing_parameter(param = "N_max", place = "spec_params")}

# if (unname(N_int) < spec_params['N_min']) {rlang::abort("N too low! N_int cannot be < N_min!", class = "error_bad_parameter")}
# if (unname(N_int) > spec_params['N_max']) {rlang::abort("N too high! N_int cannot be > N_max!", class = "error_bad_parameter")}
