#' Convert Nf to biomass
#'
#' @inheritParams Q_int
#' @inheritParams N_rel 
#' @param dry logical, return dry or wet biomass. If dry = F, spec_params['DWWW'] must be provided
#'
#' @return dry (or wet) biomass, mg m-3
#' @export
#'
#' @examples examples
#' @seealso [N_int(), biomass_to_Nf]
Nf_to_biomass <- function(Nf, Ns, N_int, dry = T, ...) {
  biomass <- (Nf + Ns) / N_int
  if (dry == F) {
    biomass <- biomass * unname(spec_params['DWWW'])
  }
  return(biomass)
}
