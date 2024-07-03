#' Plot of species growth response over a range of temperatures
#' 
#' @inheritParams T_lim
#' @param T_range a vector of the range of temperatures to be tested (defaults to seq(-10, 35, 0.25))
#'
#' @import ggplot2
#' @return a ggplot object of growth response over the specified temperature range
#' @export
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
#' @export
#'
#' @examples examples
#' @seealso [I_lim() algae_height()]
plot_I_range = function(I_range = seq(1, 2500, 1), I_top = c(1, 2, 5), spec_params, site_params){
  
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
#' @export
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

#' Title
#'
#' @param conc 
#' @param spec_params 
#' @param shape 
#'
#' @return a ggplot object of nitrogen uptake rates
#' @export
#'
#' @examples examples
plot_N_uptake = function(conc = seq(0, 25, 0.25), spec_params, shape = "Michaelis-Menton"){
  
  range.N = as.data.frame(conc)
  
  vam = mgd_umolh(spec_df$vam[spec_df$species == species])
  kam = mgm3_umolL(spec_df$kam[spec_df$species == species])
  vni = mgd_umolh(spec_df$vni[spec_df$species == species])
  kni = mgm3_umolL(spec_df$kni[spec_df$species == species])
  
  range.N = range.N %>% 
    mutate(species = species,
           ammonium = (vam * Nc)/(kam + Nc),
           nitrate = (vni * Nc)/(kni + Nc)) %>% 
    pivot_longer(names_to = "Nform", values_to = "uptake", cols = c(ammonium, nitrate))
}

  