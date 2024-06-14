temperature_year = function(t = seq(0,365,1), 
                              site_df = site_params, site = "Okehampton Bay",
                              spec_df = spec_params, species = "MAC"){
  
  T_a = as.numeric(site_df$T_a[site_df$site == site])
  T_b = as.numeric(site_df$T_b[site_df$site == site])
  T_c = as.numeric(site_df$T_c[site_df$site == site])
  
  site.T = as.data.frame(t) %>% 
    mutate(site = site,
           Tc = T_a + T_c * sin((2 * pi * (t + T_b) + pi / 2) / 365))
  
  to = as.numeric(spec_df$to[spec_df$species == species])
  ti = as.numeric(spec_df$ti[spec_df$species == species])
  ta = as.numeric(spec_df$ta[spec_df$species == species])
  
  if (to-ti > ta-to){
    year.T = site.T %>% 
      mutate(species = species,
             Tlim = case_when(Tc > ta ~ 0, Tc < ti ~ 0,
                              TRUE ~ ((Tc - ta)*(Tc - ti)^2) /
                                ((to - ti)*((to - ti)*(Tc - to) - (to - ta)*(to + ti - 2*Tc)))))
  } else {
    message("Species CTMI function not valid!")
  }
  return(year.T)
}

temperature_range = function(range.T = seq(-10,35,0.5), 
                             spec_df = spec_params, species = "MAC"){
  
  range.T = as.data.frame(range.T)
  
  to = as.numeric(spec_df$to[spec_df$species == species])
  ti = as.numeric(spec_df$ti[spec_df$species == species])
  ta = as.numeric(spec_df$ta[spec_df$species == species])
  
  if (to-ti > ta-to){
    range.T = range.T %>% 
      mutate(Tc = range.T,
             species = species,
             Tlim = case_when(Tc > ta ~ 0, Tc < ti ~ 0,
                              TRUE ~ ((Tc - ta)*(Tc - ti)^2) /
                                ((to - ti)*((to - ti)*(Tc - to) - (to - ta)*(to + ti - 2*Tc)))))
  } else {
    message("Species CTMI function not valid!")
  }
  return(range.T)
}

irradience_year = function(t = seq(0,365,1), kW = 0.58, depths = c(1, 2, 5),
                           site_df = site_params, site = "Okehampton Bay",
                           spec_df = spec_params, species = "MAC", QQ = 0.855, nf = 1000){
  
  I_a = as.numeric(site_df$I_a[site_df$site == site])
  I_b = as.numeric(site_df$I_b[site_df$site == site])
  I_c = as.numeric(site_df$I_c[site_df$site == site])
  
  site.I = as.data.frame(t) %>% 
    mutate(site = site,
           I = I_a + I_c * sin((2 * pi * (t + I_b) + pi / 2) / 365))
  
  
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
  
  year.I = list()
  for (j in 1:length(depths)){
    d_top = depths[j]
    
    year.I[[j]] = site.I %>% 
      mutate(species = species,
             i_top = I * exp(-kW * d_top),
             K = kW + (nf * acs * max(hm/d_top, 1) * 1/(min(hm, d_top))),
             Ilim = (exp(1)/(K*d_top)) * ((1/exp(i_top/io))^(1/exp(K*d_top)) - 1/exp(i_top/io)),
             depth = d_top)
  }
  year.I = bind_rows(year.I)
  return(year.I)
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

nitrogen_year = function(t = seq(0,365,1), salmon = F, spec_df = spec_params, species = "MAC"){

  site.N = as.data.frame(t) %>% 
    mutate(Am1 = 4.89154,
           Am2 = 4.89154 + 12.8 + 4.95 * sin((2 * pi * (t+60) + pi / 2) / 365),
           Ni1 = 24.2625 - 20.68 * sin((2 * pi * (t - 315) + pi / 2) / 365))
  
    vam = mgd_umolh(spec_df$vam[spec_df$species == species])
    kam = mgm3_umolL(spec_df$kam[spec_df$species == species])
    vni = mgd_umolh(spec_df$vni[spec_df$species == species])
    kni = mgm3_umolL(spec_df$kni[spec_df$species == species])
  
  year.N = site.N %>% 
    mutate(species = species,
           nitrate = (vni * Ni1)/(kni + Ni1),
           ammonium = case_when(salmon == F ~ (vam * Am1)/(kam + Am1),
                                salmon == T ~ (vam * Am2)/(kam + Am2))) %>% 
    dplyr::select(-c(Am1, Am2, Ni1)) %>% 
    pivot_longer(names_to = "Nform", values_to = "uptake", cols = c(ammonium, nitrate))
  
  return(year.N)
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

  