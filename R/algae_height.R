#' Macroalgae height
#'
#' @param Nf 
#' @param spec_params A vector of named numbers. Must include the parameters:
#' * \eqn{h_a}, \eqn{h_b} and \eqn{h_c}, parameters governing height change with \eqn{N_f}
#'
#' @return a scalar of macroalgae height (m)
#'
#' @examples examples
algae_height <- function(Nf, spec_params) {
  hm <- (Nf/spec_params['h_a'])^spec_params['h_b'] + spec_params['h_c']
  return(unname(hm))
}
