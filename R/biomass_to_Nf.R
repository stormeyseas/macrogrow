#' Convert biomass to Nf (and Ns)
#'
#' @inheritParams Nf_to_biomass
#' @inheritParams N_rel 
#' @param Q_rel the non-dimensionalised relative internal nutrient quotient (\eqn{Q_{rel}})
#' @param spec_params a vector of named numbers. Must include:
#'  * `DWWW` (if dry = F), the conversion from dry weight to wet weight
#'  * `Q_min`, the minimum internal nutrient quotient
#'  * `Q_max`, the maximum internal nutrient quotient
#' @param biomass wet (or dry) biomass, mg m-3
#'
#' @return a vector of c(Nf, Ns)
#' @export
#'
#' @examples examples
#' @seealso [N_int(), Nf_to_biomass(), ]
biomass_to_Nf <- function(biomass, N_int, Q_rel = 0.35, spec_params, dry = F) {
  if (N_int > 1) {
    N_int <- N_int/100
  }
  if (dry == F) {
    biomass <- biomass/unname(spec_params['DWWW'])
  }
  Q_int <- Q_rel * (spec_params['Q_min'] - spec_params['Q_max']) + spec_params['Q_max']
  Nf <- unname((biomass * N_int)/(1 + (Q_int/spec_params['Q_min'] - 1)))
  Ns <- unname((biomass * N_int) - Nf)
  
  return(
    c(Nf = Nf, Ns = Ns)
    )
}

