#' Macroalgae height
#'
#' @param Nf 
#' @param spec_params A vector of named numbers. Must include the parameters:
#' * `h_max`, maximum species height. If only `h_max` is supplied then algae height = `h_max`
#' * `h_a`, `h_b` and `h_c`, parameters governing height change with `N_f`
#'
#' @details
#' `h_a`, `h_b` and `h_c` default to 1000, 1 and 0 if not supplied. Algae height therefore defaults to `Nf/1000` if no parameters are supplied.
#' 
#'
#' @return a scalar of macroalgae height (m)
#' @export
#'
#' @examples examples
algae_height <- function(Nf, spec_params) {
  # Check which parameters are supplied
  
  if (!is.na(spec_params['h_max'])) {
    if (is.na(spec_params['h_a']) & is.na(spec_params['h_b']) & is.na(spec_params['h_c'])) {
      
      # h_max is present and all other variables are missing
      hm <- spec_params['h_max']
      
    } else {
      
      # h_max is present and at least one other variable is present
      if (is.na(spec_params['h_a'])) {
        h_a <- 1000
        rlang::inform("No value supplied for h_a, defaulting to 1000")
      } else {h_a <- spec_params['h_a']}
      if (is.na(spec_params['h_b'])) {
        h_b <- 1
        rlang::inform("No value supplied for h_b, defaulting to 1")
      } else {h_b <- spec_params['h_b']}
      if (is.na(spec_params['h_c'])) {
        h_c <- 0
        rlang::inform("No value supplied for h_c, defaulting to 0")
      } else {h_c <- spec_params['h_c']}
      
      hm <- (Nf/h_a)^h_b + h_c
      hm <- pmin(hm, spec_params['h_max'])
      
    }
  } else {
    
    # h_max is missing
    if (is.na(spec_params['h_a'])) {
      h_a <- 1000
      rlang::inform("No value supplied for h_a, defaulting to 1000")
    } else {h_a <- spec_params['h_a']}
    if (is.na(spec_params['h_b'])) {
      h_b <- 1
      rlang::inform("No value supplied for h_b, defaulting to 1")
    } else {h_b <- spec_params['h_b']}
    if (is.na(spec_params['h_c'])) {
      h_c <- 0
      rlang::inform("No value supplied for h_c, defaulting to 0")
    } else {h_c <- spec_params['h_c']}
    
    hm <- (Nf/h_a)^h_b + h_c
  }
  
  return(unname(hm))
}
