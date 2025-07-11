#' Macroalgae height
#'
#' @inheritParams Q_int
#' @param spec_params A vector of named numbers. Must include the parameters:
#' * `h_max`, maximum species height. Can be `NA`.
#' * `h_a`, `h_b` and `h_c`, parameters governing height change with `N_f`. If not supplied algae height will always be `h_max`.
#'
#' @details
#' Calculates macroalgae height as \eqn{h_m = \left(\frac{N_f}{h_a}\right)^{h_b} + h_c} up to a maximum of `h_max`. 
#' Defaults are \eqn{h_a=1000}, \eqn{h_b=1} and \eqn{h_c=0}. 
#' Algae height therefore defaults to \eqn{N_f \times 10^{-3}} if no parameters are supplied.
#' 
#' @return a scalar of macroalgae height (m)
#' @export
#'
#' @examples 
#' my_species <- c(h_a = 750, h_b = 0.5, h_c = 0.01, h_max = 1)
#' Nf <- seq(100, 1000, 10)
#' height <- sapply(X = Nf, FUN = height, spec_params = my_species)
#' plot(Nf, height)

height <- function(Nf, spec_params) {
  # Check which parameters are supplied
  h_a <- ifelse(is.na(spec_params['h_a']), 1000, spec_params['h_a'])
  h_b <- ifelse(is.na(spec_params['h_b']), 1000, spec_params['h_b'])
  h_c <- ifelse(is.na(spec_params['h_c']), 1000, spec_params['h_c'])
  hm <- (Nf/h_a)^h_b + h_c
  
  if (!is.na(spec_params['h_max'])) {
    hm <- min(hm, spec_params['h_max'])
  }
  return(unname(hm))
}
