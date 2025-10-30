#' Convert biomass to Nf and Ns

#' @description
#' Converts wet or dry biomass to `N_f` and `N_s` via:
#' \deqn{B = \frac{N_f + N_s}{Q_{min}}}
#' and where the ratio between `N_f` and `N_s` is calculated via:
#' \deqn{\frac{Ns}{Nf} = \frac{Q_{int}}{Q_{min}} - 1} 
#'
#' @inheritParams Nf_to_biomass
#' @inheritParams Q_rel 
#' @param biomass starting biomass, mg m-3
#' @param Q_rel the non-dimensionalised relative internal nutrient quotient (\eqn{Q_{rel}}). Only one of \eqn{Q_{int}} or \eqn{Q_{rel}} need to be provided. If neither \eqn{Q_{int}} or \eqn{Q_{rel}} are provided the default if \eqn{Q_{rel}=0.5} will be used.
#' @param spec_params a vector of named numbers. Must include:
#'  * `DWWW` (if dry = F), the conversion from dry weight to wet weight
#'  * `Q_min`, the minimum internal nutrient quotient (mg gDW-1)
#'  * `Q_max`, the maximum internal nutrient quotient
#' @param dry whether dry (default) or wet biomass is provided, mg m-3

#' @return Nf, mg m-3
#' @export
#'
#' @examples 
#' my_species <- c(DWWW = 7.5, Q_min = 20, Q_max = 45)
#' starting_biomass <- 250 # mg m-3
#' 
#' # Using default Q_rel = 0.5
#' biomass_to_Nf(biomass = starting_biomass, spec_params = my_species, dry = T)
#' 
#' # Using a specific Q_int
#' biomass_to_Nf(biomass = starting_biomass, Q_int = 30, spec_params = my_species, dry = T)
#' 
#' @seealso [Nf_to_biomass()], [Q_rel()], [Q_int()]
#' 
biomass_to_Nf <- function(biomass, Q_int = NULL, Q_rel = 0.5, spec_params, dry = T) {
  # If only Q_rel is given, convert to Q_int
  if (is.null(Q_int)) {Q_int<- Q_int(Q_rel = Q_rel, spec_params = spec_params)}

  # Biomass must be dry
  if (dry == F) {biomass <- biomass/unname(spec_params['DWWW'])}

  Nf <- biomass * unname(spec_params['Q_min']) * 10^-3 # Nf comes directly from biomass
  Nf_Ns <- Q_int / unname(spec_params['Q_min']) - 1 # Ratio of Nf to Ns
  Ns <- Nf_Ns * Nf

  return(c(Nf = Nf, Ns = Ns))
}


#' Convert Nf to biomass
#'
#' @inheritParams Q_int
#' @inheritParams Q_rel 
#' @param dry logical, return dry or wet biomass. If dry = F, `spec_params['DWWW']` must be provided
#'
#' @details
#' Not all parameters need to be provided to this function. 
#' 
#' @return dry (or wet) biomass, mg m-3
#' @export
#'
#' @examples 
#' my_species <- c(DWWW = 7.5, Q_min = 20, Q_max = 45)
#' starting_Nf <- 150 # mg m-3
#' starting_Ns <- 50 # mg m-3
#' 
#' # Using default Q_rel = 0.5
#' Nf_to_biomass(Nf = starting_Nf, Ns = starting_Ns, spec_params = my_species, dry = T)
#' 
#' # Using a specific Q_int
#' Nf_to_biomass(biomass = starting_biomass, Q_int = 30, spec_params = my_species, dry = T)
#' 
#' @seealso [biomass_to_Nf()], [Q_rel()], [Q_int()]
#' 
Nf_to_biomass <- function(Nf, Ns, Q_int = NULL, Q_rel = 0.5, spec_params, dry = T) {
  # If only Q_rel is given, convert to Q_int
  if (is.null(Q_int)) {Q_int <- Q_int(Nf = Nf, Ns = Ns, Q_rel = Q_rel, spec_params = spec_params)}
  biomass <- ((Nf + Ns) / Q_int) * 10^3
  # If biomass is dry, convert
  if (dry == F) {biomass <- biomass * unname(spec_params['DWWW'])}
  return(biomass)
}
