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
#' @importFrom nleqslv nleqslv
#' @import units
#' @export
#'
#' @examples 
#' Examples TBD
#' @seealso [Nf_to_biomass(), Q_rel(), Q_int()]
biomass_to_Nf <- function(biomass, Q_int = NULL, Q_rel = 0.5, spec_params, dry = T) {
  remove_unit("gDW")
  install_unit("gDW")

  # If only Q_rel is given convert to Q_int
  if (is.null(Q_int)) {
    Q_int <- Q_int(Q_rel = Q_rel, spec_params = spec_params)
  }
  Q_int <- set_units(Q_int, "mg gDW-1")
  Q_min <- set_units(spec_params['Q_min'], "mg gDW-1")

  # Biomass must be dry
  if (dry == F) {
    biomass <- biomass/unname(spec_params['DWWW'])
  }
  biomass <- set_units(biomass, "mgDW m-3")

  Nf <- biomass*Q_min # Nf comes directly from biomass
  Nf_Ns <- Q_int/Q_min - set_units(1, "1") # Ratio of Nf to Ns
  Ns <- Nf_Ns * Nf

  Nf <- set_units(Nf, "mg m-3") |> drop_units() |> unname()
  Ns <- set_units(Ns, "mg m-3") |> drop_units() |> unname()
  
  remove_unit("gDW")

  return(c(Nf = Nf, Ns = Ns))
}

