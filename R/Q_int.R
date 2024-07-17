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
Q_int <- function(Nf, Ns, spec_params) {
  if (is.na(spec_params['Q_min'])) {abort_missing_parameter(param = "Q_min", place = "spec_params")}
  if (Nf <= 0) {rlang::abort("Nf too low! How can fixed N (biomass) be <= 0?", class = "error_bad_parameter")}
  if (Ns < 0) {rlang::abort("Ns too low! How can stored N be < 0?", class = "error_bad_parameter")}
  
  Q_int <- spec_params['Q_min'] * (1 + Ns/Nf)
  return(unname(Q_int))
}
