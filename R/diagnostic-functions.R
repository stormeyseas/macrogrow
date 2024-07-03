#' Plot of species growth response over a range of temperatures
#' 
#' @inheritParams T_lim
#' @param T_range a vector of the range of temperatures to be tested (defaults to seq(-10, 35, 0.25))
#' @param spec_params
#'
#' @import ggplot2
#' @return a ggplot object of growth response over the specified temperature range
#' @export
#'
#' @examples
#' @seealso [T_lim()]
plot_T_range = function(T_range = seq(-10,35,0.25), spec_params){
  
  T_lim <- T_lim(T_range, spec_params)
  range <- as.data.frame(T_C = T_range, T_lim = T_lim)
  
  p <- ggplot2::ggplot(range, aes(x = T_C, y = T_lim))
  
  return(p)
}

irradience_range = function(range.I = seq(1, 2500, 1), kW = 0.58, depths = c(1, 2, 5),
                            spec_df = spec_params, species = "MAC", QQ = 0.855, nf = 1000){
  
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

height_limits = function(Nf = seq(10, 4500, 1), spec_df = spec_params, species = "MAC"){

  h_a = spec_df$h_a[spec_df$species == species]
  h_b = spec_df$h_b[spec_df$species == species]
  
  range.Nf = as.data.frame(Nf) %>% 
    mutate(species = species,
           hm = (Nf/(h_a*10^3))^h_b)
  
  return(range.Nf)
}


nitrogen_range = function(Nc = seq(0, 25, 0.25), spec_df = spec_params, species = "MAC"){
  
  range.N = as.data.frame(Nc)
  
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

  