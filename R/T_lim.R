#' Temperature limitation on growth
#' 
#' Given species parameters, returns the relative limitation on growth rate according to a CTMI curve:
#' \deqn{\begin{array}[ccc] 
#'      T_{lim} &=& \frac{(T_c-T_{max})(T_c-T_{min})^2}{(T_{opt}-T_{min})[(T_{opt}-T_{min})(T_c-T_{opt})-(T_{opt}-T_{max})(T_{opt}+T_{min}-2T_c)]}
#' \end{array}}
#' 
#' @param Tc temperature to evaluate
#' @param spec_params a vector of named numbers. Must include:
#'  * `T_opt` the optimum temperature for macroalgae growth
#'  * `T_min` the minimum temperature for macroalgae growth (when `T_c` < `T_min`, growth = 0)
#'  * `T_max` the maximum temperature for macroalgae growth (when `T_c` > `T_max`, growth = 0)
#'
#' @return a scalar of relative temperature limitation on growth (between 0 and 1)
#' @export
#'
#' @examples 
#' my_seaweed <- c(T_opt = 20, T_min = 5, T_max = 30)
#' 
#' T_lim(Tc = 22, spec_params = my_seaweed)
#' 
#' T_range <- 1:30
#' sapply(T_range, T_lim, spec_params = my_seaweed)
#' 
T_lim <- function(Tc, spec_params){
  
  # Check that required parameters are supplied
  if (is.na(spec_params['T_opt'])) {abort_missing_parameter(param = "T_opt", place = "spec_params")}
  if (is.na(spec_params['T_min'])) {abort_missing_parameter(param = "T_min", place = "spec_params")}
  if (is.na(spec_params['T_max'])) {abort_missing_parameter(param = "T_max", place = "spec_params")}
  
  if (spec_params['T_opt'] < spec_params['T_min']) {
    rlang::abort("error_bad_parameter", message = "Minimum temperature is higher than optimum temperature")
  }
  if (spec_params['T_opt'] > spec_params['T_max']) {
    rlang::abort("error_bad_parameter", message = "Error: maximum temperature is lower than optimum temperature")
  } 
  if (spec_params['T_opt']-spec_params['T_min'] <= spec_params['T_max']-spec_params['T_opt']) {
    rlang::abort("error_bad_parameter", message = "Species CTMI function not valid! Must satisfy T_opt-T_min > T_max-T_opt")
  }
  
  if (Tc >= spec_params['T_max']) {
    Tlim <- 0
  } else if (Tc <= spec_params['T_min']) {
    Tlim <- 0
  } else {
    Tlim <- ((Tc - spec_params['T_max'])*(Tc - spec_params['T_min'])^2)/((spec_params['T_opt'] - spec_params['T_min'])*((spec_params['T_opt'] - spec_params['T_min'])*(Tc - spec_params['T_opt']) - (spec_params['T_opt'] - spec_params['T_max'])*(spec_params['T_opt'] + spec_params['T_min'] - 2*Tc)))
  }
  return(unname(Tlim))
}
