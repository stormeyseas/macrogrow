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
get_uptake <- function(conc, uptake_shape = NA, Nform_abbr, spec_params) {
  
  if (is.na(conc)) {
    rlang::abort("Concentration passed to get_uptake cannot be NA", class = "error_bad_parameter")
  }
  
  M <- spec_params[paste0("M", "_", Nform_abbr)]
  C <- spec_params[paste0("C", "_", Nform_abbr)]
  V <- spec_params[paste0("V", "_", Nform_abbr)]
  K <- spec_params[paste0("K", "_", Nform_abbr)]
  
  if (is.na(uptake_shape)) {
    rlang::inform(message = glue::glue("Uptake shape for '{form}' not specified, determining based on parameters provided", form = Nform_abbr))
    if (!is.na(V) & !is.na(K)) {
      up <- MM_uptake(conc = conc, V = V, K = K)
    } else if (!is.na(M) & !is.na(C)) {
      up <- lin_uptake(conc = conc, M = M, C = C)
    } else {
      rlang::abort(glue::glue("Error! You haven't provided the correct uptake parameters for '{form}'!", form = Nform_abbr))
    }
  } else if (uptake_shape == "linear") {
    if(is.na(M)) {
      abort_missing_parameter(param = paste0("M", "_", Nform_abbr), place = "spec_params for linear uptake")
    } else if(is.na(C)) {
      abort_missing_parameter(param = paste0("C", "_", Nform_abbr), place = "spec_params for linear uptake")
    } else {
      up <- lin_uptake(conc = conc, M = M, C = C)
    }
  } else if (uptake_shape == "MM" | uptake_shape == "Michaelis-Menton") {
    if(is.na(V)) {
      abort_missing_parameter(param = paste0("V", "_", Nform_abbr), place = "spec_params for Michaelis-Menton uptake")
    } else if(is.na(K)) {
      abort_missing_parameter(param = paste0("K", "_", Nform_abbr), place = "spec_params for linear uptake")
    } else {
      up <- MM_uptake(conc = conc, V = V, K = K)
    }
  } else {
    rlang::abort(glue::glue("Uptake shape `{uptake_shape}` is not recognised in FORT KICKASS"), class = "error_bad_parameter")
  }
  return(up)
}

#' Uptake rate (linear)
#' 
#' @inheritParams MM_uptake
#' @param M the slope of N uptake with increasing substrate concentration 
#' @param C the intercept
#'
#' @return the rate of uptake at the specified external concentration
#' @export
#'
#' @examples examples
lin_uptake <- function(conc, M, C) {
  uprate <- M * conc + C
  return(unname(uprate))
}

#' Uptake rate (Michaelis-Menton)
#'
#' @param conc external substrate concentration
#' @param V the maximum uptake rate \eqn{V_{max}}
#' @param K the half-saturation constant \eqn{K_{c}}
#'
#' @return the rate of uptake at the specified external concentration
#' @export
#'
MM_uptake <- function(conc, V, K) {
  if (missing(V) & missing(K)) {abort_missing_parameter(param = "V and K", place = "spec_params. Did you mean to use lin_uptake() instead?")}
  if (missing(V)) {abort_missing_parameter(param = "V", place = "spec_params and passed to function as 'V'")}
  if (missing(K)) {abort_missing_parameter(param = "K", place = "spec_params and passed to function as 'K'")}
  
  uprate <- (V * conc / (K + conc))
  return(unname(uprate))
}
