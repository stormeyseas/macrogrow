#' Asparagopsis armata model parameters
#'
#' A named vector containing physiological and morphological parameters for the macroalgae species *Asparagopsis armata* used in the growth model.
#'
#' @format A named numeric vector with 34 elements:
#' \describe{
#'   \item{`V_am`}{Maximum ammonium uptake rate (Michaelis-Menton uptake, mg gDW-1 d-1)}
#'   \item{`K_am`}{Half-saturation constant for ammonium uptake (Michaelis-Menton uptake, mg m-3)}
#'   \item{`M_am`}{Ammonium uptake rate slope (linear uptake, m3 gDW-1 d-1)}
#'   \item{`C_am`}{Ammonium uptake rate constant (linear uptake, m3 gDW-1 d-1)}
#'   \item{`V_ni`}{Maximum nitrate uptake rate (Michaelis-Menton uptake, mg gDW-1 d-1)}
#'   \item{`K_ni`}{Half-saturation constant for nitrate uptake (Michaelis-Menton uptake, mg m-3)}
#'   \item{`M_ni`}{Nitrate uptake rate slope (linear uptake, m3 gDW-1 d-1)}
#'   \item{`C_ni`}{Nitrate uptake rate constant (linear uptake, m3 gDW-1 d-1)}
#'   \item{`V_ot`}{Maximum other nitrogen uptake rate (Michaelis-Menton uptake, mg gDW-1 d-1)}
#'   \item{`K_ot`}{Half-saturation constant for other nitrogen uptake (Michaelis-Menton uptake, mg m-3)}
#'   \item{`M_ot`}{Other nitrogen uptake rate slope (linear uptake, m3 gDW-1 d-1)}
#'   \item{`C_ot`}{Other nitrogen uptake rate constant (linear uptake, m3 gDW-1 d-1)}
#'   \item{`Q_min`}{Minimum nitrogen quotient (mg gDW-1)}
#'   \item{`Q_max`}{Maximum nitrogen quotient (mg gDW-1)}
#'   \item{`K_c`}{Half-saturation constant for growth (mg gDW-1)}
#'   \item{`mu`}{Maximum growth rate (d-1)}
#'   \item{`D_m`}{Base loss rate (d-1)}
#'   \item{`D_ve`}{Loss rate with velocity (m-1 s d-1)}
#'   \item{`D_lo`}{Loss rate at low turbidity (d-1)}
#'   \item{`D_mi`}{Loss rate at medium turbidity (d-1)}
#'   \item{`D_hi`}{Loss rate at high turbidity (d-1)}
#'   \item{`a_cs`}{Carbon-specific self-shading constant (m mg-1)}
#'   \item{`I_o`}{Light saturation parameter (μmol photons m-2 s-1)}
#'   \item{`T_opt`}{Optimal temperature (°C)}
#'   \item{`T_min`}{Minimum temperature (°C)}
#'   \item{`T_max`}{Maximum temperature (°C)}
#'   \item{`S_opt`}{Optimal salinity (g L-1)}
#'   \item{`S_min`}{Minimum salinity (g L-1)}
#'   \item{`S_max`}{Maximum salinity (g L-1)}
#'   \item{`h_a`}{Height-controlling parameter a (d-1)}
#'   \item{`h_b`}{Height-controlling parameter b (d-1)}
#'   \item{`h_c`}{Height-controlling parameter c (d-1)}
#'   \item{`h_max`}{Maximum height (m)}
#'   \item{`DWWW`}{Dry weight to wet weight ratio (gWW gDW-1)}
#' }
#'
#' @source Parameters derived from literature values and used in *Asparagopsis armata* growth modeling here (insert citation).
#'
#' @seealso [grow_macroalgae()] for the main growth model function.
#'
#' @examples
#' # View all parameters
#' a_armata
#' 
#' # Access specific parameters
#' a_armata["mu"]  # Maximum growth rate
#' a_armata["h_max"]  # Maximum height
"a_armata"

#' Default site parameters
#'
#' A named vector containing default hydrodynamic and site characteristics used in the macroalgae growth model.
#'
#' @format A named numeric vector with 4 elements:
#' \describe{
#'   \item{`hz`}{Water depth (m)}
#'   \item{`d_top`}{Distance from surface to top of the macroalgae canopy (m)}
#'   \item{`hc`}{Canopy vertical width in the water column (m)}
#'   \item{`farmA`}{Total farm area (m2)}
#' }
#'
#' @examples
#' # View all site parameters
#' site_params
#' 
#' # Access specific parameters
#' site_params["hz"]  # Water depth
#' site_params["kW"]  # Water attenuation coefficient
"site_params"

#' Example environmental time series data
#'
#' A data frame containing synthetic environmental data over a full year (365 days) with seasonal patterns and random variation. This dataset demonstrates the expected format for environmental forcing data used in the macroalgae growth model.
#'
#' @format A data frame with 365 rows and 8 columns:
#' \describe{
#'   \item{t}{Time step (day of year)}
#'   \item{temperature}{Water temperature (°C)}
#'   \item{light}{Surface light availability (μmol photons m-2 s-1)}
#'   \item{kW}{Light attenuation coefficient in water (m-1)}
#'   \item{velocity}{Mean water velocity (m s-1)}
#'   \item{salinity}{Salinity (g L-1)}
#'   \item{nitrate}{Nitrate concentration (mg m-3)}
#'   \item{ammonium}{Ammonium concentration (mg m-3)}
#' }
#'
#' @details
#' The environmental data includes:
#' - Seasonal temperature variation (15°C ± 3.5°C with random noise)
#' - Light availability with seasonal patterns
#' - Seasonal variation in light attenuation coefficient
#' - Seasonal variation in water velocity 
#' - Salinity variation around 35 g/L
#' - Nitrogen concentrations (nitrate and ammonium) with seasonal patterns
#'
#' All variables include random variation to simulate natural environmental conditions.
#'
#' @seealso [grow_macroalgae()] for using this data in growth simulations.
#'
#' @examples
#' # View structure of environmental data
#' str(env)
#' 
#' # Plot temperature over time
#' plot(env$t, env$temperature, type = "l", 
#'      xlab = "Day of year", ylab = "Temperature (°C)")
#' 
#' # Summary of all environmental variables
#' summary(env)
"env"