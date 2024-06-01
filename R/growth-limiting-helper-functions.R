#' Temperature limitation (CTMI) across time
#' 
#' Given species parameters and temperatures at each timestep (recorded or generated), returns the relative limitation on growth rate according to a CTMI curve.
#'
#' @param Tmax Maximum temperature for macroalgae growth
#' @param Topt Optimum temperature for macroalgae growth
#' @param Tmin Minimum temperature for macroalgae growth
#' @param site_df dataframe of temperatures to evaluate. First column is assumed to be timesteps and second column is assumed to be ambient temperature at that timestep.
#'
#' @return A dataframe with relative temperature limitation (between 0 and 1) at each timestep.
#' @export
#'
#' @examples
#' 
Tlim_atsite <- function(Tmax, Topt, Tmin, site_df){
  
  if (Topt < Tmin) {stop("Error: minimum temperature is higher than optimum temperature")}
  if (Topt > Tmax) {stop("Error: maximum temperature is lower than optimum temperature")}
  if (Topt-Tmin <= Tmax-Topt) {stop("Species CTMI function not valid! Must satisfy Topt-Tmin > Tmax-Topt")}
  
  colnames(site_df) = c("t", "temp")
  
  year_T = site_df %>% 
    mutate(Tlim = case_when(temp > Tmax ~ 0, temp < Tmin ~ 0,
                            TRUE ~ ((temp - Tmax)*(temp - Tmin)^2) /
                              ((Topt - Tmin)*((Topt - Tmin)*(temp - Topt) - (Topt - Tmax)*(Topt + Tmin - 2*temp)))))
  
  return(year_T)
}

#' Temperature limitation (CTMI) across temperature range
#'
#' @param Tmax Maximum temperature for macroalgae growth
#' @param Topt Optimum temperature for macroalgae growth
#' @param Tmin Minimum temperature for macroalgae growth
#' @param temp_min Minimum temperature to be tested.
#' @param temp_max Maximum temperature to be tested.
#'
#' @return A dataframe with relative temperature limitation (between 0 and 1) at each temperature.
#' @export
#'
#' @examples
#' 
Tlim_inrange <- function(Tmax, Topt, Tmin, temp_min = -5, temp_max = 40, step = 0.2){
  
  if (Topt < Tmin) {stop("Error: minimum temperature is higher than optimum temperature")}
  if (Topt > Tmax) {stop("Error: maximum temperature is lower than optimum temperature")}
  if (Topt-Tmin <= Tmax-Topt) {stop("Species CTMI function not valid! Must satisfy Topt-Tmin > Tmax-Topt")}
  
  temp_df = data.frame(temp = seq(temp_min, temp_max, step))
  
  range_T = temp_df %>% 
    mutate(Tlim = case_when(temp > Tmax ~ 0, temp < Tmin ~ 0,
                            TRUE ~ ((temp - Tmax)*(temp - Tmin)^2) /
                              ((Topt - Tmin)*((Topt - Tmin)*(temp - Topt) - (Topt - Tmax)*(Topt + Tmin - 2*temp)))))
  
  return(range_T)
}

#' Title
#'
#' @param V_am 
#' @param K_am 
#' @param V_ni 
#' @param K_ni 
#' @param site_df 
#'
#' @return
#' @export
#'
#' @examples
#' 
Nlim_atsite <- function(V_am, K_am, V_ni, K_ni, site_df){
  
  colnames(site_df) = c("t", "Nitrate", "Ammonia")
  
  year_N = site.N %>% 
    mutate(Uptake_nitrate = (V_ni * Nitrate)/(K_ni + Nitrate),
           Uptake_ammonium = (V_am * Ammonia)/(K_am + Ammonia))
  
  return(year_N)
}

#' Title
#'
#' @param V_am 
#' @param K_am 
#' @param V_ni 
#' @param K_ni 
#' @param N_min Minimum N concentration to be tested.
#' @param N_max Maximum N concentration to be tested.
#'
#' @return
#' @export
#'
#' @examples
#' 
Nlim_inrange <- function(V, K, N_min, N_max, step = 0.1){
  
  N_df = data.frame(N_conc = seq(N_min, N_max, step))
  
  range_N = N_df %>% 
    mutate(Uptake_rate = (V * N_conc)/(K + N_conc))
  
  return(range_N)
}

# Still to do:
# - height_change
# - Ilim_atsite
# - Ilim_inrange