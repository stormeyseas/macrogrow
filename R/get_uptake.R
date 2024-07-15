#' Get the correct uptake rate
#'
#' @param conc 
#' @param uptake_shape 
#' @param Nform_abbr 
#' @param spec_params 
#'
#' @export
#' 
get_uptake <- function(conc, uptake_shape, Nform_abbr, spec_params) {
  
  spec_params['M'] <- spec_params[paste0("M", "_", Nform_abbr)]
  spec_params['C'] <- spec_params[paste0("C", "_", Nform_abbr)]
  spec_params['V'] <- spec_params[paste0("V", "_", Nform_abbr)]
  spec_params['K'] <- spec_params[paste0("K", "_", Nform_abbr)]
  
  if (missing(uptake_shape)) {
    if (!is.na(spec_params['V']) & !is.na(spec_params['K'])) {
      rlang::inform(message = glue::glue("Uptake shape for '{form}' not specified, using Michaelis-Menton kinetics", form = Nform_abbr))
      MM_uptake(conc = conc, V = spec_params['V'], K = spec_params['K'])
      
    } else if (!is.na(spec_params['M']) & !is.na(spec_params['C'])) {
      rlang::inform(message = glue::glue("Uptake shape for '{form}' not specified, using linear kinetics", form = Nform_abbr))
      lin_uptake(conc = conc, M = spec_params['M'], C = spec_params['C'])
      
    } else {
      rlang::abort(glue::glue("Error! You haven't provided any uptake parameters for '{form}'!", form = Nform_abbr))
    }
  }
  
  else if (uptake_shape == "linear") {
    if(is.na(spec_params['M']) | is.na(spec_params['C'])) {
      abort_missing_parameter(param = "M and C", place = "spec_params for linear uptake")
    } else {
      lin_uptake(conc = conc, M = spec_params['M'], C = spec_params['C'])
    }
  } 
  
  # uptake_shape == "MM" | uptake_shape == "Michaelis-Menton"
  else {
    if(is.na(spec_params['V']) | is.na(spec_params['K'])) {
      abort_missing_parameter(param = "V and K", place = "spec_params for linear uptake")
    } else {
      MM_uptake(conc = conc, V = spec_params['V'], K = spec_params['K'])
    }
  } 
}
