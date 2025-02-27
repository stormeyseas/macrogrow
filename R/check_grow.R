#' Check grow
#' 
#' @description
#' Check that all parameters, inputs and settings are correct for the `grow_macroalgae()` function. Gives a report on what needs to be included. This is to avoid the main function slowing down to give endless warnings and messages. 
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
#'
#' @examples "see here" link?
check_grow <- function(start, grow_days, temperature, salinity, light, velocity, nitrate, ammonium, other_N = NA, ni_uptake, am_uptake, ot_uptake = NA, site_params, spec_params, initials, sparse_output = T, other_constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)) {
  rlang::inform("Starting all checks...")
  
  # Start date
  if (!lubridate::is.Date(start)) {
    rlang::inform("Variable 'start' is not a date. Convert to date using lubridate::parse_date_time() or similar.")
  }
  if (is.na(grow_days) | missing(grow_days)) {
    rlang::inform("Variable 'grow_days' not provided. Length of input variable 'temperature' will be used to populate timeseries.")
  } else if (!is.integer(grow_days) | grow_days <= 0) {
    rlang::inform("Variable 'grow_days' must be a positive integer. Only whole days can be used.")
  }
  
  # Check that input variables are present
  if (!is.numeric(temperature)) {rlang::inform("FATAL: Variable 'temperature' must be a numeric vector.")}
  if (!is.numeric(salinity)) {rlang::inform("FATAL: Variable 'salinity' must be a numeric vector.")}
  if (!is.numeric(light)) {rlang::inform("FATAL: Variable 'light' must be a numeric vector.")}
  if (!is.numeric(velocity)) {rlang::inform("FATAL: Variable 'velocity' must be a numeric vector.")}
  if (!is.numeric(nitrate)) {rlang::inform("FATAL: Variable 'nitrate' must be a numeric vector.")}
  if (!is.numeric(ammonium)) {rlang::inform("FATAL: Variable 'ammonium' must be a numeric vector.")}
  
  if (!is.na(other_N)) {
    use_other_N <- F
    check_length <- c(length(temperature), length(salinity), length(light), length(velocity), length(nitrate), length(ammonium))
  } else if (!is.numeric(other_N)) {
    use_other_N <- T
    rlang::inform("FATAL: Variable 'other_N' must be a numeric vector. If you do not want to use uptake of other_N, set other_N = NA.")
    check_length <- c(length(temperature), length(salinity), length(light), length(velocity), length(nitrate), length(ammonium), length(other_N))
  }
  
  # Check that input variables are the correct length
  if (var(check_length) != 0) {
    rlang::inform("FATAL: Input variables are not all the same length.")
  } else if ((check_length[1] - 1) == grow_days) {
    rlang::inform("WARN: Variable 'grow_days' is one day shorter than input variables. grow_macroalgae() will add 1 day to grow_days to match input variables (it's inclusive of planting and harvest days).")
  } else if (check_length[1] != grow_days) {
    rlang::inform("FATAL: Variable 'grow_days' must be equal to the length of input variables (inclusive of planting and harvest days).")
  }

  essential_site_params <- c("farmA", "hz", "hc", "kW", "d_top")
  if (!all(essential_site_params %in% names(site_params))) {
    rlang::inform(paste0("FATAL: Parameter '", essential_site_params[which(!essential_site_params %in% names(site_params))], "' is missing from site_params."))
  }
  
  if (is.na(initials['Nf'])) {rlang::inform("FATAL: Parameter 'Nf' is missing from initials. Calculate it using biomass_to_Nf() first if required.")}
  if (is.na(initials['Q_int']) & is.na(initials['Q_rel'])) {rlang::inform("FATAL: Parameters 'Q_int' and 'Q_rel' are missing from initials. You must provide one of them to get macroalgae initial state. Calculate Q_int using Q_int() from Nf and Ns.")}
  
  essential_spec_params <- c('Q_min', 'Q_max', 'K_c', 'mu', 'a_cs', 'I_o', 'T_opt', 'T_min', 'T_max', 'DWWW')
  if (!all(essential_spec_params %in% names(spec_params))) {
    rlang::inform(paste0("FATAL: Parameter '", essential_spec_params[which(!essential_spec_params %in% names(spec_params))], "' is missing from spec_params."))
  }
  
  # Optional parts, which depend on other inputs -------------------------------------------------------------------------------------------------------
  if (use_other_N == T) {
    if (is.na(ot_uptake) & all(c("V_ot", "K_ot", "M_ot", "K_ot") %in% names(spec_params))) {
      rlang::inform("WARN: spec_params has provided parameters for Michaelis-Menton and linear uptake for other_N. Uptake will default to Michaelis-Menton kinetics.")
    } else if (ot_uptake == "MM" & all(!c("V_ot", "K_ot") %in% names(spec_params))) {
      rlang::inform("FATAL: Variable 'ot_uptake' (uptake of other_N) is set to Michaelis-Menton kinetics but required parameters are not provided. 'V_ot' and 'K_ot' must be provided in spec_params.")
    } else if (ot_uptake == "MM" & all(!c("V_ot", "K_ot") %in% names(spec_params))) {
      rlang::inform("FATAL: Variable 'ot_uptake' (uptake of other_N) is set to linear kinetics but required parameters are not provided. 'M_ot' and 'C_ot' must be provided in spec_params.")
    }
  } else {
    rlang::inform("WARN: Variable 'other_N' not provided - no uptake of 'other' N sources will occur.")
  }
  
  if (is.na(ni_uptake) & all(c("V_ni", "K_ni", "M_ni", "K_ni") %in% names(spec_params))) {
    rlang::inform("WARN: spec_params has provided parameters for Michaelis-Menton and linear uptake for nitrate. Uptake will default to Michaelis-Menton kinetics.")
  } else if (ni_uptake == "MM" & all(!c("V_ni", "K_ni") %in% names(spec_params))) {
    rlang::inform("FATAL: Variable 'ni_uptake' (uptake of nitrate) is set to Michaelis-Menton kinetics but required parameters are not provided. 'V_ni' and 'K_ni' must be provided in spec_params.")
  } else if (ni_uptake == "MM" & all(!c("V_ni", "K_ni") %in% names(spec_params))) {
    rlang::inform("FATAL: Variable 'ni_uptake' (uptake of nitrate) is set to linear kinetics but required parameters are not provided. 'M_ni' and 'C_ni' must be provided in spec_params.")
  }
  
  if (is.na(am_uptake) & all(c("V_am", "K_am", "M_am", "K_am") %in% names(spec_params))) {
    rlang::inform("WARN: spec_params has provided parameters for Michaelis-Menton and linear uptake for ammonium. Uptake will default to Michaelis-Menton kinetics.")
  } else if (am_uptake == "MM" & all(!c("V_am", "K_am") %in% names(spec_params))) {
    rlang::inform("FATAL: Variable 'am_uptake' (uptake of ammonium) is set to Michaelis-Menton kinetics but required parameters are not provided. 'V_am' and 'K_am' must be provided in spec_params.")
  } else if (am_uptake == "MM" & all(!c("V_am", "K_am") %in% names(spec_params))) {
    rlang::inform("FATAL: Variable 'am_uptake' (uptake of ammonium) is set to linear kinetics but required parameters are not provided. 'M_am' and 'C_am' must be provided in spec_params.")
  }
  
  if (is.na(site_params['turbulence'])) {rlang::inform("WARN: Parameter 'turbulence' is missing from site_params. Turbulence loss will not work.")}
  
  
  # "D_m"
  # "D_ve"
  # "D_lo"
  # "D_mi"
  # "D_hi"
  if (is.na(spec_params['S_opt'])) {rlang::inform("FATAL: Parameter 'S_opt' is missing from spec_params")}
  if (is.na(spec_params['S_min'])) {rlang::inform("FATAL: Parameter 'S_min' is missing from spec_params")}
  if (is.na(spec_params['S_max'])) {rlang::inform("FATAL: Parameter 'S_max' is missing from spec_params")}
  # "h_a"
  # "h_b"
  # "h_c"
  # "h_max"

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


