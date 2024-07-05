#' Q limitation on growth
#'
#' @description Calculates the internal nutrient quotient \eqn{Q} and its relative effect on growth rate \eqn{Q_{lim}} via:
#' \deqn{\begin{array}[ccc] 
#' Q &=& Q_{min} \left(1 + \frac{N_s}{N_f}\right)
#' Q_{lim} &=& \frac{Q - Q_{min}}{Q - K_c}
#' \end{array}}
#' 
#' @inheritParams Q
#' @inheritParams QQ
#' 
#' @return a scalar of relative limitation from internal nutrient reserves on growth (between 0 and 1)
#' @export
#' 
#' @examples examples
#' 
Q_lim <- function(Nf, Ns, spec_params) {
  Q <- spec_params['Q_min'] * (1 + Ns/Nf)
  if (Q < spec_params['Q_min']) {Q <- spec_params['Q_min']} 
  if (Q > spec_params['Q_max']) {Q <- spec_params['Q_max']}
  
  Q_lim <- (Q - spec_params['Q_min'])/(Q - spec_params['K_c'])
  return(Q_lim)
}
