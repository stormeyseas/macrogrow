#' @title Pre-check all parameters for grow_macroalgae()
#' 
#' @description
#' Check that all parameters, inputs and settings are correct for the `grow_macroalgae()` function. Gives a report on what needs to be modified for the main function to run smoothly. This is to avoid the main function slowing down to give endless warnings and messages. 
#'
#' @inheritParams grow_macroalgae
#'
#' @importFrom lubridate is.Date ymd duration yday parse_date_time
#' @importFrom glue glue
#' @import rlang cli
#' @importFrom units set_units drop_units
#' 
#' @return printout of potential errors for main function
#' @export 
#' 
#' @details
#' Example csv with all the spec_params & site_params required?
#'
#' @examples "see here" link?
check_grow <- function(
    t = 1:30,
    temperature,
    salinity,
    light,
    kW,
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
  if (any(!is.integer(t), !is.numeric(t))) {
    inform(c("x" = "Variable 't' must be a vector of integers, or numbers coercible to integers. (Hint: Convert dates using lubridate::yday)"))
  } else {
    inform(c("v" = "Timeseries looks good."))
  }

  # Check that input variables are present and look good
  helpcheck(t, temperature, "temperature")
  helpcheck(t, salinity, "salinity")
  helpcheck(t, light, "light")
  helpcheck(t, kW, "kW")
  helpcheck(t, light, "light")
  helpcheck(t, velocity, "velocity")
  helpcheck(t, nitrate, "nitrate")
  helpcheck(t, ammonium, "ammonium")

  # Check that site params is good
  essential_site_params <- c("farmA", "hz", "hc", "d_top")
  if (!all(essential_site_params %in% names(site_params))) {
    rlang::inform(paste0("x" = "Parameter '", essential_site_params[which(!essential_site_params %in% names(site_params))], "' is missing from site_params."))
  } else if (any(is.na(site_params[essential_site_params]))) {
    rlang::inform(paste0("x" = "Parameter '", essential_site_params[which(is.na(site_params[essential_site_params]))], "' in site_params cannot be NA."))
  } else {
    inform(c("v" = "Site params looks good."))
  }
  
  # Check validity of initial variables
  if (!'Nf' %in% names(initials)) {
    rlang::inform("x" = "Parameter 'Nf' is missing from initials. Calculate it using biomass_to_Nf() first if required.")
  } else if (is.na(initials['Nf'])) {
    rlang::inform("x" = "Parameter 'Nf' in initials cannot be NA.")
  } else {
    inform(c("v" = "Variable Nf in initials looks good."))
  }
  
  if (!'Q_int' %in% names(initials) & !'Q_rel' %in% names(initials)) {
    rlang::inform("x" = "Parameters 'Q_int' and 'Q_rel' are missing from initials. You must provide one of them to get macroalgae initial state. You can calculate Q_int using Q_int() from Nf and Ns.")
  } else if (is.na(initials['Q_int']) & is.na(initials['Q_rel'])) {
    rlang::inform("x" = "Parameters 'Q_int' and 'Q_rel' in initials cannot both be NA.")
  } else {
    inform(c("v" = "Variable Q_int/Q_rel in initials looks good."))
  }
  
  # Check that all essential species parameters are present
  essential_spec_params <- c('Q_min', 'Q_max', 'K_c', 'mu', 'a_cs', 'I_o', 'T_opt', 'T_min', 'T_max', 'DWWW')
  if (!all(essential_spec_params %in% names(spec_params))) {
    rlang::inform(paste0("x" = "Parameter '", essential_spec_params[which(!essential_spec_params %in% names(spec_params))], "' is missing from spec_params."))
  } else if (any(is.na(spec_params[essential_spec_params]))) {
    rlang::inform(paste0("x" = "Parameter '", essential_spec_params[which(is.na(spec_params[essential_spec_params]))], "' in spec_params cannot be NA."))
  } else {
    ess_spec_params <- T
  }
  
  # Optional parts, which depend on other inputs -------------------------------------------------------------------------------------------------------  
  # Validate nitrogen uptake parameters
  validate_uptake_params(ni_uptake, spec_params, "nitrate", "ni")
  validate_uptake_params(am_uptake, spec_params, "ammonium", "am")
  
  if (is.na(site_params['turbulence'])) {rlang::inform(
    ">" = "Parameter 'turbulence' is missing from site_params. Turbulence loss will not work regardless of spec_params supplied.")
  }
  
  
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
    rlang::inform(">" = "sparse_output is 'F', full outputs will be given.")
  } else {
    rlang::inform(">" = "sparse_output is 'T', truncated outputs will be given.")
  }
}

#' Check input vector compatibility with time series
#'
#' @description
#' Internal helper function that validates input vectors against the time series vector.
#' Performs comprehensive checks for NULL values, numeric type, length compatibility,
#' and missing values. Used by [`check_grow()`](R/check_grow.R:20) to validate all input variables.
#'
#' @param t Numeric vector representing the time series
#' @param vec Numeric vector to be validated against the time series
#' @param name Character string specifying the variable name for error messages
#'
#' @return Invisible boolean indicating validation success (TRUE) or failure (FALSE)
#' @keywords internal
#'
#' @details
#' The function performs the following validation steps:
#' \itemize{
#'   \item Checks for NULL values in inputs
#'   \item Validates that vec is numeric
#'   \item Ensures vec length matches t length
#'   \item Handles missing values using [`handle_missing_values()`](R/check_grow.R:183)
#' }
#'
#' @seealso [`handle_missing_values()`](R/check_grow.R:183), [`check_grow()`](R/check_grow.R:20)
helpcheck <- function(t, vec, name) {
  # Input validation for edge cases
  if (is.null(vec)) {
    rlang::inform(c("x" = glue::glue("Variable '{name}' is NULL.")))
    return(invisible(FALSE))
  }
  
  if (is.null(t) || length(t) == 0) {
    rlang::inform(c("x" = "Timeseries vector 't' is NULL or empty."))
    return(invisible(FALSE))
  }
  
  if (is.null(name) || nchar(name) == 0) {
    rlang::inform(c("x" = "Variable name is NULL or empty."))
    return(invisible(FALSE))
  }
  
  # Check if vector is numeric
  if (!is.numeric(vec)) {
    rlang::inform(c("x" = glue::glue("Variable '{name}' must be a numeric vector.")))
    return(invisible(FALSE))
  }
  
  # Check length compatibility
  vec_length <- length(vec)
  t_length <- length(t)
  
  if (vec_length != t_length) {
    rlang::inform(c("x" = glue::glue(
      "Variable '{name}' length ({vec_length}) does not match timeseries vector 't' length ({t_length})."
    )))
    return(invisible(FALSE))
  }
  
  # Check for missing values and handle based on variable importance
  na_count <- sum(is.na(vec))
  if (na_count > 0) {
    handle_missing_values(name, na_count, vec_length)
    return(invisible(FALSE))
  }
  
  # All checks passed
  rlang::inform(c("v" = glue::glue("Variable '{name}' looks good.")))
  return(invisible(TRUE))
}

#' Handle missing value messages based on variable type
#'
#' @description
#' Internal helper function that generates appropriate warning messages for missing values
#' based on the variable type and importance. Categorizes variables as essential or optional
#' and provides context-specific guidance for handling missing data.
#'
#' @param name Character string specifying the variable name
#' @param na_count Integer number of missing values in the variable
#' @param total_length Integer total length of the variable vector
#'
#' @return No return value. Function is called for its side effects of printing informative messages.
#' @keywords internal
#'
#' @details
#' Variables are categorized as:
#' \itemize{
#'   \item \strong{Essential variables}: temperature, ammonium, nitrate - cannot have missing values
#'   \item \strong{Optional variables}: salinity, light, kW, velocity - missing values disable specific limitations
#'   \item \strong{Other variables}: general warning about potential model performance impact
#' }
#'
#' The function calculates the percentage of missing values and provides variable-specific
#' guidance on the consequences of missing data.
#'
#' @seealso [`helpcheck()`](R/check_grow.R:136)
handle_missing_values <- function(name, na_count, total_length) {
  # Define variable categories and their handling
  essential_vars <- c("temperature", "ammonium", "nitrate")
  optional_vars <- list(
    "salinity" = "Salinity limitation will not be factored into growth.",
    "light" = "Light limitation will not be factored into growth.",
    "kW" = "Light limitation will not be factored into growth.",
    "velocity" = "Biomass loss due to current speed will not be factored into growth."
  )
  
  na_percentage <- round(na_count / total_length * 100, 1)
  base_message <- glue::glue("Variable '{name}' has {na_count} missing values.")
  
  if (name %in% essential_vars) {
    rlang::inform(c("x" = glue::glue(
      "{base_message} This input is essential and cannot have missing values."
    )))
  } else if (name %in% names(optional_vars)) {
    rlang::inform(c(">" = glue::glue(
      "{base_message} {optional_vars[[name]]} To use this limitation, ensure the input vector has no missing values."
    )))
  } else {
    rlang::inform(c(">" = glue::glue(
      "{base_message} This may affect model performance."
    )))
  }
}



#' Validate uptake parameters for nitrate or ammonium
#'
#' @param uptake_type Character string specifying uptake type ("MM" for Michaelis-Menton, "linear" for linear, or NA)
#' @param spec_params Named list containing species parameters
#' @param nutrient_name Character string for the nutrient name (e.g., "nitrate", "ammonium")
#' @param param_suffix Character string for parameter suffix (e.g., "ni", "am")
#'
#' @return Invisible boolean indicating success
#' @keywords internal
validate_uptake_params <- function(uptake_type, spec_params, nutrient_name, param_suffix) {
  # Define required parameters for each uptake type
  mm_params <- paste0(c("V_", "K_"), param_suffix)
  linear_params <- paste0(c("M_", "C_"), param_suffix)
  
  # Helper function to check if all parameters are present
  has_all_params <- function(params) {
    all(all(params %in% names(spec_params)), !is.na(spec_params[params]))
  }
  
  # Handle different uptake type scenarios
  if (is.na(uptake_type)) {
    # Auto-detect uptake type based on available parameters
    has_mm <- has_all_params(mm_params)
    has_linear <- has_all_params(linear_params)
    
    if (has_mm && has_linear) {
      rlang::inform(c(">" = glue::glue(
        "spec_params has provided parameters for both Michaelis-Menton and linear uptake for {nutrient_name}. ",
        "Uptake will default to Michaelis-Menton kinetics."
      )))
      return(invisible(TRUE))
    } else if (has_mm) {
      rlang::inform(c(">" = glue::glue(
        "spec_params has provided Michaelis-Menton parameters for {nutrient_name}. ",
        "Uptake will use Michaelis-Menton kinetics."
      )))
      return(invisible(TRUE))
    } else if (has_linear) {
      rlang::inform(c(">" = glue::glue(
        "spec_params has provided linear parameters for {nutrient_name}. ",
        "Uptake will use linear kinetics."
      )))
      return(invisible(TRUE))
    } else {
      rlang::inform(c("x" = glue::glue(
        "No uptake parameters provided for {nutrient_name}. ",
        "Either ({paste(mm_params, collapse = ', ')}) for Michaelis-Menton or ",
        "({paste(linear_params, collapse = ', ')}) for linear uptake must be provided in spec_params."
      )))
      return(invisible(FALSE))
    }
  } else if (uptake_type == "MM") {
    # Validate Michaelis-Menton parameters
    if (!has_all_params(mm_params)) {
      missing_params <- mm_params[!mm_params %in% names(spec_params)]
      rlang::inform(c("x" = glue::glue(
        "Variable '{param_suffix}_uptake' (uptake of {nutrient_name}) is set to Michaelis-Menton kinetics ",
        "but required parameters are missing: {paste(missing_params, collapse = ', ')}. ",
        "All parameters ({paste(mm_params, collapse = ', ')}) must be provided in spec_params."
      )))
      return(invisible(FALSE))
    }
  } else if (uptake_type == "linear") {
    # Validate linear parameters
    if (!has_all_params(linear_params)) {
      missing_params <- linear_params[!linear_params %in% names(spec_params)]
      rlang::inform(c("x" = glue::glue(
        "Variable '{param_suffix}_uptake' (uptake of {nutrient_name}) is set to linear kinetics ",
        "but required parameters are missing: {paste(missing_params, collapse = ', ')}. ",
        "All parameters ({paste(linear_params, collapse = ', ')}) must be provided in spec_params."
      )))
      return(invisible(FALSE))
    }
  } else {
    # Invalid uptake type
    rlang::inform(c("x" = glue::glue(
      "Invalid uptake type '{uptake_type}' for {nutrient_name}. ",
      "Must be 'MM' for Michaelis-Menton, 'linear' for linear kinetics, or NA for auto-detection."
    )))
    return(invisible(FALSE))
  }
  
  # All validations passed
  rlang::inform(c("v" = glue::glue("Variable '{param_suffix}_uptake' parameters look good.")))
  return(invisible(TRUE))
}
