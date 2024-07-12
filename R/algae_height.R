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
  # Check that required parameters are supplied
  if (is.na(spec_params['h_a'])) {abort_missing_parameter(param = "h_a", place = "spec_params")}
  if (is.na(spec_params['h_b'])) {
    h_b <- 1
    rlang::inform(message = "h_b not supplied, defaulting to 1")
  } else {h_b <- spec_params['h_b']}
  if (is.na(spec_params['h_c'])) {
    h_c <- 0
    rlang::inform(message = "h_c not supplied, defaulting to 0")
  } else {h_c <- spec_params['h_c']}
  
  hm <- (Nf/spec_params['h_a'])^h_b + h_c
  return(unname(hm))
}
