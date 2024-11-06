#' Convert Nf to biomass
#'
#' @inheritParams Q_int
#' @inheritParams Q_rel 
#' @param dry logical, return dry or wet biomass. If dry = F, spec_params['DWWW'] must be provided
#'
#' @return dry (or wet) biomass, mg m-3
#' @export
#'
#' @examples examples
#' @seealso [biomass_to_Nf(), Q_rel(), Q_int()]
Nf_to_biomass <- function(Nf, Ns, Q_int = NULL, Q_rel = 0.5, spec_params, dry = T) {
  if (is.null(Q_int)) {
    Q_int <- unname(Q_rel * (spec_params['Q_min'] - spec_params['Q_max']) + spec_params['Q_max'])
  }
  biomass <- 10^3 * (Nf + Ns) / Q_int
  if (dry == F) {
    biomass <- biomass * unname(spec_params['DWWW'])
  }
  return(biomass)
}
