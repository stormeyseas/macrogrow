#' Light limitation on growth
#' 
#' @description
#' Calculates the relative limitation on growth rate due to light availability, via:
#' \deqn{I_{lim} = \frac{e}{K \cdot d_{top}} \times \Biggl[e^{-\frac{I_z e^{-K \cdot d_{top}}}{I_o}} - e^{-\frac{I_z}{I_o}} \Biggr]}
#' where \eqn{I_{z}=I e^{-k_W \cdot d_{top}}} is the irradiance at the cultivation depth 
#' and \eqn{K=k_{m}+kW} is the total attenuation coefficient. 
#' 
#' \eqn{k_{m}} is the additional attenuation coefficient from macroalgae biomass, calculated as:
#' \deqn{k_{m} = a_{cs} \times N_f \times \text{max} \Biggl( \frac{h_{m}}{d_{top}}, 1 \Biggr) \times \frac{1}{\text{min}(h_{m}, d_{top})}}
#' where \eqn{h_m} is the macroalgae height. 
#' 
#' @inheritParams Q_int
#' @param I the surface irradiance, PAR (\eqn{\mu}mol photons m\eqn{^{-2}} s\eqn{^{-1}})
#' @param kW the light attenuation coefficient for open water (m\eqn{^{-1}})
#' @param spec_params a vector of named numbers. Must include:
#' * `a_cs`, the carbon-specific self-shading constant
#' * `I_o`, the light saturation parameter
#' * `h_a`, `h_b` and `h_c`, parameters governing height change with `N_f`
#' * `h_max`, maximum species height
#' @param site_params A vector of named numbers. Must include:
#' * `d_top`, the below-surface  depth (m) of the top of the macroalgae culture
#'
#' @return a scalar of relative light limitation on growth (between 0 and 1)
#' @export
#'
#' @examples 
#' my_species <- c(I_o = 200, a_cs = 0.001, h_a = 750, h_b = 0.5, h_c = 0.01, h_max = 1)
#' my_site <- c(d_top = 1)
#' I <- seq(0, 2000, 10)
#' kW <- rnorm(length(I), 0.05, 0.01)
#' 
#' # Use purrr::map2 to calculate light limitation over a range of light and attenuation values, but fixed Nf
#' \dontrun{
#' df <- purrr::map2_dfr(I, kW, function(Iz, kWz) {
#'   data.frame(
#'     I = Iz,
#'     kW = kWz,
#'     lim = I_lim(Nf = 1000, I = Iz, kW = kWz, spec_params = my_species, site_params = my_site)
#'   )
#' })
#' }
#' 
#' @seealso [macrogrow::algae_height()]
#' 
I_lim <- function(Nf, I, kW, spec_params, site_params) {
  # Check that required parameters are supplied
  if (is.na(site_params['d_top'])) {abort_missing_parameter(param = "d_top", place = "site_params")}
  if (is.na(spec_params['a_cs'])) {abort_missing_parameter(param = "a_cs", place = "spec_params")}
  if (is.na(spec_params['I_o'])) {abort_missing_parameter(param = "I_o", place = "spec_params")}
  
  I_top <- I * exp(-(kW*site_params['d_top']))
  
  h_m <- height(Nf, spec_params)
  k_ma <-  Nf * h_m * spec_params['a_cs'] * pmax(h_m/site_params['d_top'], 1) * 1/(pmin(h_m, site_params['d_top']))
  K <- k_ma + kW
  
  Ilim <- exp(1)/(K*h_m) *
    (
      exp(-(I_top*exp(-K*h_m))/spec_params['I_o']) - exp(-I_top/spec_params['I_o'])
      )
  
  return(unname(Ilim))
}
