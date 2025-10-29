#' Convert biomass to Nf and Ns

#' @description
#' B = (N_f + N_s)/Q_min
#' Q_rel = Q_min * (1 + N_s/N_f)
#'
#' @inheritParams Nf_to_biomass
#' @inheritParams Q_rel 
#' @param Q_rel the non-dimensionalised relative internal nutrient quotient (\eqn{Q_{rel}}). If neither of Q_int and Q_rel are provided the default Q_rel of 0.5 will be used.
#' @param spec_params a vector of named numbers. Must include:
#'  * `DWWW` (if dry = F), the conversion from dry weight to wet weight
#'  * `Q_min`, the minimum internal nutrient quotient (mg gDW-1)
#'  * `Q_max`, the maximum internal nutrient quotient
#' @param biomass whether dry (default) or wet biomass is provided, mg m-3
#'
#' @return Nf, mg m-3
#' @export
#'
#' @examples 
#' my_species <- c(DWWW = 7.5, Q_min = 20, Q_max = 45)
#' starting_biomass <- 250 # mg m-3
#' 
#' # Using default Q_rel = 0.5
#' \dontrun{Nf_to_biomass(biomass = starting_biomass, spec_params = my_species, dry = T)}
#' 
#' # Using a specific Q_int
#' \dontrun{Nf_to_biomass(biomass = starting_biomass, Q_int = 30, spec_params = my_species, dry = T)}
#' 
#' @seealso [macrogrow::Nf_to_biomass()], [macrogrow::Q_rel()], [macrogrow::Q_int()]
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
#' @param dry logical, return dry or wet biomass. If dry = F, spec_params['DWWW'] must be provided
#'
#' @return dry (or wet) biomass, mg m-3
#' @export
#'
#' @examples 
#' my_species <- c(DWWW = 7.5, Q_min = 20, Q_max = 45)
#' starting_biomass <- 250 # mg m-3
#' 
#' # Using default Q_rel = 0.5
#' \dontrun{Nf_to_biomass(biomass = starting_biomass, spec_params = my_species, dry = T)}
#' 
#' # Using a specific Q_int
#' \dontrun{Nf_to_biomass(biomass = starting_biomass, Q_int = 30, spec_params = my_species, dry = T)}
#' 
#' @seealso [macrogrow::biomass_to_Nf()], [macrogrow::Q_rel()], [macrogrow::Q_int()]
#' 
Nf_to_biomass <- function(Nf, Ns, Q_int = NULL, Q_rel = 0.5, spec_params, dry = T) {
  # If only Q_rel is given, convert to Q_int
  if (is.null(Q_int)) {Q_int <- Q_int(Nf = Nf, Ns = Ns, Q_rel = Q_rel, spec_params = spec_params)}
  biomass <- ((Nf + Ns) / Q_int) * 10^3
  # If biomass is dry, convert
  if (dry == F) {biomass <- biomass * unname(spec_params['DWWW'])}
  return(biomass)
}
