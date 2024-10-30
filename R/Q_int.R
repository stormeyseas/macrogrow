#' Non-dimensionalised internal nutrient quotient
#' 
#' @description Calculates the internal nutrient quotient \eqn{Q} via:
#' \deqn{\begin{array}[ccc] 
#' Q &=& Q_{min} \left(1 + \frac{N_s}{N_f}\right)
#' \end{array}}
#'
#' @param Nf Fixed nitrogen (mg m\eqn{^{-3}})
#' @param Ns Stored nitrogen (mg m\eqn{^{-3}})
#' @param spec_params A vector of named numbers. Must include:
#'  * `Q_min`, the minimum internal nutrient quotient
#'
#' @return the non-dimensionalised internal nutrient quotient
#' @export
#' @seealso [N_int(), N_rel(), Q_rel()]
#'
#' @examples examples
Q_int <- function(Nf = NULL, Ns = NULL, Q_int = NULL, N_int = NULL, Q_rel = NULL, N_rel = NULL, spec_params) {
  if (!is.null(N_int)) {N_rel <- N_rel(N_int = N_int, spec_params = spec_params)}
  if (!is.null(N_rel)) {Q_rel <- Q_rel(N_rel = N_rel, spec_params = spec_params)}
  if (!is.null(Q_rel)) {
    Q_int <- spec_params['Q_max'] - (1 - Q_rel)*(spec_params['Q_max'] - spec_params['Q_min'])
  } else if (!is.null(Nf) & !is.null(Ns)) {
    Q_int <- spec_params['Q_min'] * (1 + Ns/Nf)
  }
  if (Q_int > spec_params['Q_max']) {Q_int <- spec_params['Q_max']}
  if (Q_int < spec_params['Q_min']) {Q_int <- spec_params['Q_min']}
  # if (is.na(spec_params['Q_min'])) {abort_missing_parameter(param = "Q_min", place = "spec_params")}
    # if (Nf <= 0) {rlang::abort("Nf too low! How can fixed N (biomass) be <= 0?", class = "error_bad_parameter")}
    # if (Ns < 0) {rlang::abort("Ns too low! How can stored N be < 0?", class = "error_bad_parameter")}
    # 
    # if (is.na(spec_params['Q_max'])) {abort_missing_parameter(param = "Q_max", place = "spec_params if Q_int is supplied")}
  
  return(unname(Q_int))
}

