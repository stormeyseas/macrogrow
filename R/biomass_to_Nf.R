#' Convert biomass to Nf (and Ns)
#'
#' @inheritParams Nf_to_biomass
#' @inheritParams N_rel 
#' @param biomass wet (or dry) biomass, mg m-3
#'
#' @return a vector of c(Nf, Ns)
#'
#' @examples examples
#' @seealso [N_int(), Nf_to_biomass]
biomass_to_Nf <- function(biomass, N_int, spec_params, dry = T) {
  if (dry == F) {
    biomass <- biomass/unname(spec_params['DWWW'])
  }
  Nf_Ns <- biomass * N_int
  
  return(biomass)
}
