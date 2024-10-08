#' Get the correct uptake rate
#'
#' @param conc substrate concentration
#' @param uptake_shape kinetic shape for substrate uptake. One of "linear" or "Michaelis-Menton" (or "MM"). Defaults to Michaelis-Menton. 
#' @param spec_params a vector of named numbers. Must include:
#' * `M` and `C` for linear uptake, OR
#' * `V` and `K` for Michaelis-Menton uptake
#' * `uptake_shape` of "linear" or "MM" (or "Michaelis-Menton"). Not required if only one set of the above parameters are provided.
#' @param Nform_abbr the abbreviation used in spec_params for the relevant nitrogen form. See details.
#'
#' @details
#' Gives the substrate uptake rate based on the shape and species-specific parameters supplied. 
#' If no shape is supplied, attempts to infer the correct shape from supplied parameters. 
#' 
#' `Nform_abbr` allows the function to link species parameters with concentration. 
#' E.g. if `Nform_abbr` = "amm" (for ammonium) the function will look for `M_amm` and `C_amm` or `V_amm` and `K_amm` in spec_params and will ignore other uptake parameters which may be included for other substrates. 
#' 
#' @export
#' @seealso [lin_uptake(), MM_uptake()]
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
