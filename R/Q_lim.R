#' Q_int limitation on growth
#'
#' @description Calculates the internal nutrient quotient \eqn{Q_{int}} and its relative effect on growth rate \eqn{Q_{lim}} via:
#' \deqn{\begin{array}[ccc] 
#' Q_{int} &=& Q_{min} \left(1 + \frac{N_s}{N_f}\right)
#' Q_{lim} &=& \frac{Q_{int} - Q_{min}}{Q_{int} - K_c}
#' \end{array}}
#' 
#' @inheritParams Q_int
#' @inheritParams Q_rel
#' 
#' @return a scalar of relative limitation from internal nutrient reserves on growth (between 0 and 1)
#' @export
#' 
#' @examples examples
#' 
Q_lim <- function(Nf, Ns, spec_params) {
  Q_int <- Q_int(Nf, Ns, spec_params)
  if (spec_params['K_c'] >= spec_params['Q_min']) {rlang::abort("K_c must be less than Q_min", class = "error_bad_parameter")}
  if (Q_int < spec_params['Q_min']) {Q_int <- spec_params['Q_min']} 
  if (Q_int > spec_params['Q_max']) {Q_int <- spec_params['Q_max']}
  
  Q_lim <- (Q_int - spec_params['Q_min'])/(Q_int - spec_params['K_c'])
  return(unname(Q_lim))
}
