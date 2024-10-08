#' Plot of species growth response over a range of temperatures
#' 
#' @inheritParams T_lim
#' @param T_range a vector of the range of temperatures to be tested (defaults to seq(-10, 35, 0.25))
#'
#' @import ggplot2
#' @return a ggplot object of growth response over the specified temperature range
#'
#' @examples examples
#' @seealso [T_lim()]
plot_T_range = function(T_range = seq(-10,35,0.25), spec_params){
  
  T_lim <- T_lim(T_range, spec_params)
  range <- as.data.frame(T_C = T_range, T_lim = T_lim)
  
  p <- ggplot2::ggplot(range, aes(x = T_C, y = T_lim))
  
  return(p)
}

#' Plot of species growth response over a range of surface irradience values
#'
#' @param I_range 
#' @param kW 
#' @param I_top 
#' @param QQ 
#' @param nf 
#'
#' @return a ggplot object of growth response over the specified range of surface irradience values
#'
#' @examples examples
#' @seealso [I_lim() algae_height()]
plot_I_range = function(I_range = seq(1, 2500, 1), I_top = c(1, 2, 5), spec_params, site_params){
  
  T_lim <- I_lim(Nf, I_top, spec_params, site_params)
  range <- as.data.frame(T_C = T_range, T_lim = T_lim)
  
  p <- ggplot2::ggplot(range, aes(x = T_C, y = T_lim))
  
  return(p)
  site.I = as.data.frame(range.I) %>% 
    mutate(Ic = range.I)
  
  acs = as.numeric(spec_df$acs[spec_df$species == species])
  nmax = as.numeric(spec_df$nmax[spec_df$species == species])
  nmin = as.numeric(spec_df$nmin[spec_df$species == species])
  h_a = as.numeric(spec_df$h_a[spec_df$species == species])
  h_b = as.numeric(spec_df$h_b[spec_df$species == species])
  h_c = as.numeric(spec_df$h_c[spec_df$species == species])
  dwww = as.numeric(spec_df$dwww[spec_df$species == species])
  io = as.numeric(spec_df$io[spec_df$species == species])
  
  hm = (((nf)/(h_a*10^3))^(h_b))
  Hf = nperc = nmax - QQ*(nmax - nmin)
  B.ww.mg = dwww * nf/nperc
  
  range.I = list()
  for (j in 1:length(depths)){
    d_top = depths[j]
    
    range.I[[j]] = site.I %>% 
      mutate(species = species,
             i_top = Ic * exp(-kW * d_top),
             K = kW + (nf * acs * max(hm/d_top, 1) * 1/(min(hm, d_top))),
             Ilim = (exp(1)/(K*d_top)) * ((1/exp(i_top/io))^(1/exp(K*d_top)) - 1/exp(i_top/io)),
             depth = d_top)
  }
  range.I = bind_rows(range.I)
  return(range.I)
}

#' Plot height change over a range of fixed-N values
#'
#' @param Nf 
#' @param spec_params 
#'
#' @return a ggplot object of growth response over the specified range of fixed-N values
#'
#' @examples examples
plot_h = function(Nf = seq(1, 4500, 1), spec_params){

  h_a = spec_df$h_a[spec_df$species == species]
  h_b = spec_df$h_b[spec_df$species == species]
  
  range.Nf = as.data.frame(Nf) %>% 
    mutate(species = species,
           hm = (Nf/(h_a*10^3))^h_b)
  
  return(range.Nf)
}

#' Plot a species uptake rate over a range of N concentrations
#'
#' @param conc a vector of the range of N concentrations to be tested (defaults to seq(0, 50, 0.25))
#' @param spec_params a vector of named numbers. Must include parameters for the appropriate form as:
#' * \eqn{V_{max}}, the maximum uptake rate and \eqn{K_{c}}, the half-saturation constant 
#' AND/OR
#' * M, the slope of N uptake with increasing substrate concentration and C, the intercept 
#' @param form form of nitrogen taken up, to correspond to given parameters. One of "ammonium" (default), "nitrate", or "other".
#' @param shape one of "Michaelis-Menton" (or "MM", default), "linear", or "both".
#'
#' @return a ggplot object of nitrogen uptake rates
#'
#' @examples examples
plot_N_uptake = function(conc = seq(0, 50, 0.25), spec_params){
  
  up_Am <- 
    if(is.na(spec_params['M_am']) & is.na(spec_params['C_am'])) {
      MM_uptake(conc = conc, V = spec_params['V_am'], K = spec_params['K_am'])
    } else if (is.na(spec_params['V_am']) & is.na(spec_params['K_am'])) {
      lin_uptake(conc = conc, M = spec_params['M_am'], C = spec_params['C_am'])
    } else if (uptake_ammonium == "MM") {
      stop("Error: parameters for ammonium uptake missing! Michaelis-Menton uptake needs V_am and K_am")
    } else if (uptake_ammonium == "linear") {
      stop("Error: parameters for ammonium uptake missing! Linear uptake needs M_am and C_am")
    } else {
      stop("Error: Unknown uptake of ammonium. Specify uptake shape or provide only one set of parameters (V and K for MM, M and C for linear).")
    }
  
  up_Ni <- 
    if(is.na(spec_params['M_am']) & is.na(spec_params['C_am'])) {
      MM_uptake(conc = conc, V = spec_params['V_ni'], K = spec_params['K_ni']) * Q * B_dw.mg/1000
    } else if (is.na(spec_params['V_ni']) & is.na(spec_params['K_ni'])) {
      lin_uptake(conc = conc, M = spec_params['M_ni'], C = spec_params['C_ni']) * Q * B_dw.mg/1000
    } else if (uptake_nitrate == "MM") {
      stop("Error: parameters for nitrate uptake missing! Michaelis-Menton uptake needs V_am and K_am")
    } else if (uptake_nitrate == "linear") {
      stop("Error: parameters for nitrate uptake missing! Linear uptake needs M_am and C_am")
    } else {
      stop("Error: Unknown uptake of nitrate. Specify uptake shape or provide only one set of parameters (V and K for MM, M and C for linear).")
    }
  
  N_range = data.frame(conc = conc, up_Am = up_Am, up_Ni = up_Ni)

  range.N = range.N %>% 
    mutate(species = species,
           ammonium = (vam * conc)/(kam + Nc),
           niT_rangeate = (vni * Nc)/(kni + Nc)) %>% 
    pivot_longer(names_to = "Nform", values_to = "uptake", cols = c(ammonium, niT_rangeate))
}

  