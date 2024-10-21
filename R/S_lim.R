#' Salinity limitation on growth
#' 
#' Given species parameters, returns the relative limitation on growth rate according to a CTMI curve:
#' \deqn{
#'      S_{lim} &=& \frac{(Sal-S_{max})(Sal-S_{min})^2}{(S_{opt}-S_{min})[(S_{opt}-S_{min})(Sal-S_{opt})-(S_{opt}-S_{max})(S_{opt}+S_{min}-2Sal)]}
#' }
#' The CTMI curve was formulated for temperature.
#' 
#' @param Sal salinity to evaluate (g/L)
#' @param spec_params a vector of named numbers. Must include:
#'  * `S_opt` the optimum salinity for macroalgae growth
#'  * `S_min` the minimum salinity for macroalgae growth (when `Sal` <= `S_min`, growth = 0)
#'  * `S_max` the maximum salinity for macroalgae growth (when `Sal` >= `S_max`, growth = 0)
#'
#' @return a scalar of relative salinity limitation on growth (between 0 and 1)
#' @export
#'
#' @examples 
#' my_seaweed <- c(S_opt = 20, S_min = 5, S_max = 30)
#' 
#' S_lim(Sal = 22, spec_params = my_seaweed)
#' 
#' S_range <- 1:30
#' sapply(S_range, S_lim, spec_params = my_seaweed)
#' 
S_lim <- function(Sal, spec_params){
  
  # Check that required parameters are supplied
  if (is.na(spec_params['S_opt'])) {abort_missing_parameter(param = "S_opt", place = "spec_params")}
  if (is.na(spec_params['S_min'])) {abort_missing_parameter(param = "S_min", place = "spec_params")}
  if (is.na(spec_params['S_max'])) {abort_missing_parameter(param = "S_max", place = "spec_params")}
  
  if (spec_params['S_opt'] < spec_params['S_min']) {rlang::abort("error_bad_parameter", message = "Minimum salinity is higher than optimum salinity")}
  if (spec_params['S_opt'] > spec_params['S_max']) {rlang::abort("error_bad_parameter", message = "Error: maximum salinity is lower than optimum salinity")} 
  if (spec_params['S_opt']-spec_params['S_min'] <= spec_params['S_max']-spec_params['S_opt']) {rlang::abort("error_bad_parameter", message = "Species CTMI function not valid! Must satisfy S_opt-S_min > S_max-S_opt")}
  
  if (Sal >= spec_params['S_max']) {
    Slim <- 0
  } else if (Sal <= spec_params['S_min']) {
    Slim <- 0
  } else {
    Slim <- ((Sal - spec_params['S_max'])*(Sal - spec_params['S_min'])^2)/((spec_params['S_opt'] - spec_params['S_min'])*((spec_params['S_opt'] - spec_params['S_min'])*(Sal - spec_params['S_opt']) - (spec_params['S_opt'] - spec_params['S_max'])*(spec_params['S_opt'] + spec_params['S_min'] - 2*Sal)))
  }
  return(unname(Slim))
}
