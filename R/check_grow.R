#' @title Pre-check all parameters for grow_macroalgae()
#' 
#' @description
#' Check that all parameters, inputs and settings are correct for the `grow_macroalgae()` function. Gives a report on what needs to be modified for the main function to run smoothly. This is to avoid the main function slowing down to give endless warnings and messages. 
#'
#' @inheritParams grow_macroalgae()
#'
#' @importFrom lubridate is.Date ymd duration yday parse_date_time
#' @importFrom glue glue
#' @import rlang cli
#' @importFrom units set_units drop_units
#' 
#' @return printout of potential errors with main function
#' @export 
#' 
#' @details
#' Example csv with all the spec_params & site_params required?
#'
#' @examples "see here" link?
check_grow <- function(
    start,
    grow_days,
    temperature,
    salinity,
    light,
    velocity,
    nitrate,
    ammonium,
        ni_uptake,
    am_uptake,
        site_params,
    spec_params,
    initials,
    sparse_output = T,
    other_constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)
  ) {
  rlang::inform("Starting all checks...")
  
  # Start date
  if (!is.integer(start) & !is.numeric(start)) {
    inform(c("x" = "Variable 'start' is not an integer. Convert a date to numeric using lubridate::yday() or similar, or use the default value of 1."))
  } else if (is.numeric(start)) {
    inform(c(">" = paste0("Variable 'start' is numeric (", start, ") but will be converted to integer (", as.integer(start), ")")))
  } else if (is.integer(start)) {
    inform(c("v" = "Variable 'start' looks good."))
  }
  
  if (is.na(grow_days) | missing(grow_days)) {
    rlang::inform(">" = "Variable 'grow_days' not provided. Length of input variable 'temperature' will be used to populate timeseries.")
  } else if (!is.integer(grow_days) | grow_days <= 0) {
    rlang::inform("x" = "Variable 'grow_days' must be a positive integer. Only whole days can be used.")
  }
  
  # Check that input variables are present
  if (!is.numeric(temperature)) {rlang::inform("x" = "Variable 'temperature' must be a numeric vector.")}
  if (!is.numeric(salinity)) {rlang::inform("x" = "Variable 'salinity' must be a numeric vector.")}
  if (!is.numeric(light)) {rlang::inform("x" = "Variable 'light' must be a numeric vector.")}
  if (!is.numeric(velocity)) {rlang::inform("x" = "Variable 'velocity' must be a numeric vector.")}
  if (!is.numeric(nitrate)) {rlang::inform("x" = "Variable 'nitrate' must be a numeric vector.")}
  if (!is.numeric(ammonium)) {rlang::inform("x" = "Variable 'ammonium' must be a numeric vector.")}
  
  if (is.na(other_N)) {
    use_other_N <- F
    check_length <- c(length(temperature), length(salinity), length(light), length(velocity), length(nitrate), length(ammonium))
  } else if (!is.numeric(other_N)) {
    use_other_N <- T
    rlang::inform(">" = "Variable 'other_N' is set to NA")
    check_length <- c(length(temperature), length(salinity), length(light), length(velocity), length(nitrate), length(ammonium), length(other_N))
  }
  
  # Check that input variables are the correct length
  t <- seq(start, (start+grow_days-1), 1)
  if (var(check_length) != 0) {
    rlang::inform("x" = "Input variables are not all the same length.")
  } else if (check_length[1] == length(t)+1) {
    rlang::inform(paste0(">" = "Variable 'grow_days' = ", grow_days, " but length of input variables = ", check_length[1], ". grow_macroalgae() will add 1 day to grow_days to match input variables (it's inclusive of planting and harvest days)."))
  } else if (check_length[1] != length(t)) {
    rlang::inform(paste0(">" = "Variable 'grow_days' = ", grow_days, " but length of input variables = ", check_length[1], ". These must match - 'grow_days' is inclusive of planting AND harvest days."))
  }

  essential_site_params <- c("farmA", "hz", "hc", "d_top")
  if (!all(essential_site_params %in% names(site_params))) {
    rlang::inform(paste0("x" = "Parameter '", essential_site_params[which(!essential_site_params %in% names(site_params))], "' is missing from site_params."))
  } else if (any(is.na(site_params[essential_site_params]))) {
    rlang::inform(paste0("x" = "Parameter '", essential_site_params[which(is.na(site_params[essential_site_params]))], "' in site_params cannot be NA."))
  }
  
  # Check validity of initial variables
  if (!'Nf' %in% names(initials)) {
    rlang::inform("x" = "Parameter 'Nf' is missing from initials. Calculate it using biomass_to_Nf() first if required.")
  } else if (is.na(initials['Nf'])) {
    rlang::inform("x" = "Parameter 'Nf' in initials cannot be NA.")
  }
  
  if (!'Q_int' %in% names(initials) & !'Q_rel' %in% names(initials)) {
    rlang::inform("x" = "Parameters 'Q_int' and 'Q_rel' are missing from initials. You must provide one of them to get macroalgae initial state. You can calculate Q_int using Q_int() from Nf and Ns.")
  } else if (is.na(initials['Q_int']) & is.na(initials['Q_rel'])) {
    rlang::inform("x" = "Parameters 'Q_int' and 'Q_rel' in initials cannot both be NA.")
  }
  
  # Check that all essential parameters are present
  essential_spec_params <- c('Q_min', 'Q_max', 'K_c', 'mu', 'a_cs', 'I_o', 'T_opt', 'T_min', 'T_max', 'DWWW')
  if (!all(essential_spec_params %in% names(spec_params))) {
    rlang::inform(paste0("x" = "Parameter '", essential_spec_params[which(!essential_spec_params %in% names(spec_params))], "' is missing from spec_params."))
  } else if (any(is.na(spec_params[essential_spec_params]))) {
    rlang::inform(paste0("x" = "Parameter '", essential_spec_params[which(is.na(spec_params[essential_spec_params]))], "' in spec_params cannot be NA."))
  }
  
  # Optional parts, which depend on other inputs -------------------------------------------------------------------------------------------------------
    if (is.na(ni_uptake) & all(c("V_ni", "K_ni", "M_ni", "K_ni") %in% names(spec_params))) {
    rlang::inform(">" = "spec_params has provided parameters for Michaelis-Menton and linear uptake for nitrate. Uptake will default to Michaelis-Menton kinetics.")
  } else if (ni_uptake == "MM" & all(!c("V_ni", "K_ni") %in% names(spec_params))) {
    rlang::inform("x" = "Variable 'ni_uptake' (uptake of nitrate) is set to Michaelis-Menton kinetics but required parameters are not provided. 'V_ni' and 'K_ni' must be provided in spec_params.")
  } else if (ni_uptake == "MM" & all(!c("V_ni", "K_ni") %in% names(spec_params))) {
    rlang::inform("x" = "Variable 'ni_uptake' (uptake of nitrate) is set to linear kinetics but required parameters are not provided. 'M_ni' and 'C_ni' must be provided in spec_params.")
  }
  
  if (is.na(am_uptake) & all(c("V_am", "K_am", "M_am", "K_am") %in% names(spec_params))) {
    rlang::inform(">" = "spec_params has provided parameters for Michaelis-Menton and linear uptake for ammonium. Uptake will default to Michaelis-Menton kinetics.")
  } else if (am_uptake == "MM" & all(!c("V_am", "K_am") %in% names(spec_params))) {
    rlang::inform("x" = "Variable 'am_uptake' (uptake of ammonium) is set to Michaelis-Menton kinetics but required parameters are not provided. 'V_am' and 'K_am' must be provided in spec_params.")
  } else if (am_uptake == "MM" & all(!c("V_am", "K_am") %in% names(spec_params))) {
    rlang::inform("x" = "Variable 'am_uptake' (uptake of ammonium) is set to linear kinetics but required parameters are not provided. 'M_am' and 'C_am' must be provided in spec_params.")
  }
  
  if (is.na(site_params['turbulence'])) {rlang::inform(">" = "Parameter 'turbulence' is missing from site_params. Turbulence loss will not work.")}
  
  
  # "D_m"
  # "D_ve"
  # "D_lo"
  # "D_mi"
  # "D_hi"
  if (is.na(spec_params['S_opt'])) {rlang::inform("x" = "Parameter 'S_opt' is missing from spec_params")}
  if (is.na(spec_params['S_min'])) {rlang::inform("x" = "Parameter 'S_min' is missing from spec_params")}
  if (is.na(spec_params['S_max'])) {rlang::inform("x" = "Parameter 'S_max' is missing from spec_params")}
  if (is.na(spec_params['h_max'])) {rlang::inform("x" = "Parameter 'h_max' is missing from spec_params")}
  if (any(is.na(c(spec_params['h_a'], spec_params['h_b'], spec_params['h_c'])))) {
    rlang::inform(">" = "Some height parameters are missing from spec_params. Unless supplied, parameters will default to h_a = 1000, h_b = 1, h_c = 0.")
  }
  
  # Get checks and warnings from u_c()
  # Get checks and warnings from T_lim()
  # Get checks and warnings from S_lim()
  # Get checks and warnings from Q_lim()
  # Get checks and warnings from I_lim()
  # Get checks and warnings from loss()
  # Get checks and warnings from get_uptake()
  
  if(!is.logical(sparse_output)) {
    rlang::inform("x" = "sparse_output is not logical - must be 'T' (default) or 'F'.")
  } else if (sparse_output == F) {
    rlang::inform("INFORM: sparse_output is 'F', full outputs will be given.")
  } else {
    rlang::inform("INFORM: sparse_output is 'T', truncated outputs will be given.")
  }
}


