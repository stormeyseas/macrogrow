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
#' height <- sapply(X = Nf, FUN = algae_height, spec_params = my_species)
#' plot(Nf, height)

algae_height <- function(Nf, spec_params) {
  # Check which parameters are supplied

  # Maximum height supplied
  if (!is.na(spec_params['h_max'])) {
    
    # No other parameters supplied
    if (is.na(spec_params['h_a']) & is.na(spec_params['h_b']) & is.na(spec_params['h_c'])) {
      hm <- spec_params['h_max']
    } else {
      
      # h_max is present and at least one other variable is present
      if (is.na(spec_params['h_a'])) {
        h_a <- 1000
        # rlang::inform("No value supplied for h_a, defaulting to 1000")
      } else {h_a <- spec_params['h_a']}
      if (is.na(spec_params['h_b'])) {
        h_b <- 1
        # rlang::inform("No value supplied for h_b, defaulting to 1")
      } else {h_b <- spec_params['h_b']}
      if (is.na(spec_params['h_c'])) {
        h_c <- 0
        # rlang::inform("No value supplied for h_c, defaulting to 0")
      } else {h_c <- spec_params['h_c']}
      
      hm <- (Nf/h_a)^h_b + h_c
      hm <- pmin(hm, spec_params['h_max'])
      
    }
  } else {
    
    # h_max is missing
    if (is.na(spec_params['h_a'])) {
      h_a <- 1000
      # rlang::inform("No value supplied for h_a, defaulting to 1000")
    } else {h_a <- spec_params['h_a']}
    if (is.na(spec_params['h_b'])) {
      h_b <- 1
      # rlang::inform("No value supplied for h_b, defaulting to 1")
    } else {h_b <- spec_params['h_b']}
    if (is.na(spec_params['h_c'])) {
      h_c <- 0
      # rlang::inform("No value supplied for h_c, defaulting to 0")
    } else {h_c <- spec_params['h_c']}
    
    hm <- (Nf/h_a)^h_b + h_c

  }
  
  return(unname(hm))
}
