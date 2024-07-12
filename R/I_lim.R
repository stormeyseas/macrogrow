#' Light limitation on growth
#' 
#' @description
#' Calculates the relative limitation on growth rate due to light availability, via:
#' \deqn{\begin{array}[ccc] 
#' I_{lim} &=& \frac{e}{K d_{top}} \times \Biggl[e^{-\frac{I_z e^{-K d_{top}}}{I_o}} - e^{-\frac{I_z}{I_o}} \Biggr]
#' \end{array}}
#' where \eqn{$I_{z}=I e^{-k_W \cdot d_{top}}$} is the irradiance at the cultivation depth,
#' and \eqn{K=k_{m}+kW} is the total attenuation coefficient.
#' \eqn{k_{m}} is the additional attenuation coefficient from macroalgae biomass, calculated as:
#' \deqn{\begin{array}[ccc] 
#' k_{m} &=& a_{cs} \times N_f \times \text{max} \Biggl( \frac{h_{m}}{d_{top}}, 1 \Biggr) \times \frac{1}{\text{min}(h_{m}, d_{top})}
#' \end{array}}
#' where \eqn{h_{m}} is the macroalgae height.
#' 
#' @inheritParams Q_int
#' @param I the surface irradiance, PAR (\eqn{\mu}mol photons m\eqn{^{-2}} s\eqn{^{-1}})
#' @param spec_params a vector of named numbers. Must include:
#'    * \eqn{a_{cs}}, the carbon-specific self-shading constant
#'    * \eqn{I_o}, the light saturation parameter
#'    * \eqn{h_a}, \eqn{h_b} and \eqn{h_c}, parameters governing height change with \eqn{N_f}
#' @param site_params A vector of named numbers. Must include:
#'    * \eqn{d_{top}} the below-surface  depth (m) of the top of the macroalgae culture
#'    * \eqn{kW} the light-attenuation coefficient of open water
#'
#' @return a scalar of relative light limitation on growth (between 0 and 1)
#' @export
#'
#' @examples examples
#' 
#' @seealso [algae_height()]
I_lim <- function(Nf, I, spec_params, site_params) {
  # Check that required parameters are supplied
  if (is.na(site_params['kW'])) {abort_missing_parameter(param = "kW", place = "site_params")}
  if (is.na(site_params['d_top'])) {abort_missing_parameter(param = "d_top", place = "site_params")}
  if (is.na(spec_params['a_cs'])) {abort_missing_parameter(param = "a_cs", place = "spec_params")}
  if (is.na(spec_params['I_o'])) {abort_missing_parameter(param = "I_o", place = "spec_params")}
  if (is.na(spec_params['h_a'])) {abort_missing_parameter(param = "h_a", place = "spec_params")}
  if (is.na(spec_params['h_b'])) {
    h_b <- 1
    rlang::inform(message = "h_b not supplied, defaulting to 1")
  }
  if (is.na(spec_params['h_c'])) {
    h_c <- 0
    rlang::inform(message = "h_c not supplied, defaulting to 0")
  }
  
  I_top <- I * exp(-(site_params['kW']*site_params['d_top']))
  
  h_m <- algae_height(Nf, spec_params)
  k_ma <-  Nf * h_m * spec_params['a_cs'] * pmax(h_m/site_params['d_top'], 1) * 1/(pmin(h_m, site_params['d_top']))
  K <- k_ma + site_params['kW']
  
  Ilim <- exp(1)/(K*h_m) *
    (
      exp(-(I_top*exp(-K*h_m))/spec_params['I_o']) - exp(-I_top/spec_params['I_o'])
      )
  
  return(unname(Ilim))
}
