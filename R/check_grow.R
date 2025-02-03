#' Check grow
#' 
#' @description
#' Check that all parameters, inputs and settings are correct for the `grow_macroalgae()` function. Gives a report on what needs to be included. This is to avoid the main function giving endless warnings and messages. 
#'
#' @param start date, start of the growth period, date of at-sea deployment
#' @param grow_days integer, number of day in growing period - if missing will take the length of the temperature vector
#' @param temperature a vector of daily temperatures (C)
#' @param salinity a vector of daily salt concentrations (g L-1)
#' @param light a vector of incoming light (umol m-2 s-1)
#' @param velocity a vector of water velocities (m s-1)
#' @param nitrate a vector of nitrate concentrations (mg m-3)
#' @param ammonium a vector of ammonium concentrations (mg m-3)
#' @param other_N a vector of other nitrogen concentrations (mg N m-3) 
#' @param site_params a named vector of species-specific parameters - see details
#' @param spec_params a named vector of site-specific parameters - see details
#' @param initials a named vector of the macroalgae starting conditions
#' @param sparse_output logical, whether to include input vectors and other non-essential information in final dataframe (default = TRUE)
#' @param other_constants a named vector of miscellaneous constants (see u_c)
#'
#' @importFrom lubridate is.Date ymd duration yday parse_date_time
#' @importFrom glue glue
#' @import rlang
#' @importFrom units set_units drop_units
#' 
#' @return dataframe of outputs
#' @export 
#' 
#' @details
#' Example csv with all the spec_params & site_params required? 
#' - Note that the final growth dataframe is inclusive of the start and end date, so the environmental vectors must be the same
#'
#' @examples "see here" link?
check_grow <- function(
    start, grow_days, temperature, salinity, light, velocity, nitrate, ammonium, 
    other_N = NA, ni_uptake, am_uptake, # ot_uptake, 
    site_params, spec_params, initials, sparse_output = T, other_constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)) {
  # Start date
  if (!lubridate::is.Date(start)) {
    rlang::inform("Variable 'start' is not a date. Convert to date using lubridate::parse_date_time() or similar.")
  } else {
    rlang::inform("Variable 'start' looks good.")
  }
  
  if (is.na(grow_days) | missing(grow_days)) {
    rlang::inform("Variable 'grow_days' not provided. Length of input variables will be used to populate timeseries.")
  } else if (!is.integer(grow_days) | grow_days <= 0) {
    rlang::inform("Variable 'grow_days' must be a positive integer. Only whole days can be used.")
  } else {
    rlang::inform("Variable 'grow_days' looks good.")
  }
  
  # Check that input variables are present
  if (!is.numeric(temperature)) {rlang::inform("Variable 'temperature' must be a numeric vector.")}
  if (!is.numeric(salinity)) {rlang::inform("Variable 'salinity' must be a numeric vector.")}
  if (!is.numeric(light)) {rlang::inform("Variable 'light' must be a numeric vector.")}
  if (!is.numeric(velocity)) {rlang::inform("Variable 'velocity' must be a numeric vector.")}
  if (!is.numeric(nitrate)) {rlang::inform("Variable 'nitrate' must be a numeric vector.")}
  if (!is.numeric(ammonium)) {rlang::inform("Variable 'ammonium' must be a numeric vector.")}
  if (is.na(other_N)) {
    rlang::inform("Variable 'other_N' not provided - no uptake of 'other' N sources will occur.")
    check_length <- c(length(temperature), length(salinity), length(light), length(velocity), length(nitrate), length(ammonium))
  } else if (!is.numeric(other_N)) {
    rlang::inform("Variable 'other_N' must be a numeric vector.")
    check_length <- c(length(temperature), length(salinity), length(light), length(velocity), length(nitrate), length(ammonium), length(other_N))
  }
  
  # Check that input variables are the correct length (WORKING)
  if (var(check_length) != 0) {
    rlang::inform("Input variables are not all the same length. Inputs must be the same length.")
  }
  if (length(temperature) == grow_days) {
    rlang::inform("Variable 'grow_days' is the same as.")
  }

  if (is.na(site_params['farmA'])) {rlang::inform("FATAL: Parameter 'farmA' is missing from site_params.")}  
  if (is.na(site_params['hc'])) {rlang::inform("FATAL: Parameter 'hc' is missing from site_params.")}  
  if (is.na(site_params['kW'])) {rlang::inform("FATAL: Parameter 'kW' is missing from site_params.")}  
  if (is.na(site_params['d_top'])) {rlang::inform("FATAL: Parameter 'd_top' is missing from site_params.")}  
  if (is.na(site_params['turbulence'])) {rlang::inform("WARN: Parameter 'turbulence' is missing from site_params. Turbulence loss will not work.")}
  
  if (is.na(initials['Nf'])) {rlang::inform("FATAL: Parameter 'Nf' is missing from initials.")}  
  if (is.na(initials['Q_int']) & is.na(initials['Q_rel'])) {rlang::inform("FATAL: Parameters 'Q_int' and 'Q_rel' are missing from initials. Must provide one of them to get macroalgae initial state.")}
  
  # "V_am"
  # "K_am"
  # "M_am"
  # "C_am"
  # "V_ni"
  # "K_ni"
  # "M_ni"
  # "C_ni"
  # "V_ot"
  # "K_ot"
  # "M_ot"
  # "C_ot"
  if (is.na(spec_params['Q_min'])) {rlang::inform("FATAL: Parameter 'Q_min' is missing from spec_params")}
  if (is.na(spec_params['Q_max'])) {rlang::inform("FATAL: Parameter 'Q_max' is missing from spec_params")}
  if (is.na(spec_params['K_c'])) {rlang::inform("FATAL: Parameter 'K_c' is missing from spec_params")}
  if (is.na(spec_params['mu'])) {rlang::inform("FATAL: Parameter 'mu' is missing from spec_params")}
  # "D_m"
  # "D_ve"
  # "D_lo"
  # "D_mi"
  # "D_hi"
  if (is.na(spec_params['a_cs'])) {rlang::inform("FATAL: Parameter 'a_cs' is missing from spec_params")}
  if (is.na(spec_params['I_o'])) {rlang::inform("FATAL: Parameter 'I_o' is missing from spec_params")}
  if (is.na(spec_params['T_opt'])) {rlang::inform("FATAL: Parameter 'T_opt' is missing from spec_params")}
  if (is.na(spec_params['T_min'])) {rlang::inform("FATAL: Parameter 'T_min' is missing from spec_params")}
  if (is.na(spec_params['T_max'])) {rlang::inform("FATAL: Parameter 'T_max' is missing from spec_params")}
  if (is.na(spec_params['S_opt'])) {rlang::inform("FATAL: Parameter 'S_opt' is missing from spec_params")}
  if (is.na(spec_params['S_min'])) {rlang::inform("FATAL: Parameter 'S_min' is missing from spec_params")}
  if (is.na(spec_params['S_max'])) {rlang::inform("FATAL: Parameter 'S_max' is missing from spec_params")}
  # "h_a"
  # "h_b"
  # "h_c"
  # "h_max"
  if (is.na(spec_params['DWWW'])) {rlang::inform("FATAL: Parameter 'DWWW' is missing from spec_params")}
  
  # Get checks and warnings from algae_height()
  # Get checks and warnings from u_c()
  # Get checks and warnings from T_lim()
  # Get checks and warnings from S_lim()
  # Get checks and warnings from Q_lim()
  # Get checks and warnings from I_lim()
  # Get checks and warnings from loss()
  # Get checks and warnings from get_uptake()
  
  if(!is.logical(sparse_output)) {
    rlang::inform("FATAL: sparse_output is not logical - must be 'T' (default) or 'F'.")
  } else if (sparse_output == F) {
    rlang::inform("INFORM: sparse_output is 'F', full outputs will be given.")
  } else {
    rlang::inform("INFORM: sparse_output is 'T', truncated outputs will be given.")
  }
}


