#' Convert biomass to Nf (and Ns)
#'
#' @inheritParams Nf_to_biomass
#' @inheritParams Q_rel 
#' @param Q_rel the non-dimensionalised relative internal nutrient quotient (\eqn{Q_{rel}})
#' @param spec_params a vector of named numbers. Must include:
#'  * `DWWW` (if dry = F), the conversion from dry weight to wet weight
#'  * `Q_min`, the minimum internal nutrient quotient
#'  * `Q_max`, the maximum internal nutrient quotient
#' @param biomass wet (or dry) biomass, mg m-3
#'
#' @return Nf, mg m-3
#' @export
#'
#' @examples 
#' Examples TBD
#' @seealso [Nf_to_biomass(), Q_rel(), Q_int()]
biomass_to_Nf <- function(biomass, Q_int = NULL, Q_rel = 0.5, spec_params, dry = T) {
  if (is.null(Q_int)) {
    Q_int <- unname(Q_rel * (spec_params['Q_min'] - spec_params['Q_max']) + spec_params['Q_max'])
  }
  if (dry == F) {
    biomass <- biomass/unname(spec_params['DWWW'])
  }
  Nf <- unname((biomass * (Q_int/1000))/(1 + (Q_int/spec_params['Q_min'] - 1)))
  
  return(Nf)
}

