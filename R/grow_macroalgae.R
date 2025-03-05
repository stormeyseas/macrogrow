#' Grow macroalgae
#' 
#' @description
#' Initiate and grow macroalgae.
#'
#' @param start date, start of the growth period, date of at-sea deployment
#' @param grow_days integer, number of day in growing period - if missing will take the length of the temperature vector
#' @param temperature a vector of daily temperatures (C)
#' @param salinity a vector of daily salt concentrations (g L-1)
#' @param light a vector of incoming light (umol m-2 s-1)
#' @param velocity a vector of water velocities (m s-1)
#' @param nitrate a vector of nitrate concentrations (mg m-3)
#' @param ammonium a vector of ammonium concentrations (mg m-3)
#' @param other_N a vector of other nitrogen concentrations (mg N m-3) 
#' @param site_params a named vector of species-specific parameters - see details
#' @param spec_params a named vector of site-specific parameters - see details
#' @param initials a named vector of the macroalgae starting conditions
#' @param sparse_output logical, whether to include input vectors and other non-essential information in final dataframe (default = TRUE)
#' @param other_constants a named vector of miscellaneous constants (see u_c)
#'
#' @importFrom lubridate is.Date ymd duration yday parse_date_time
#' @importFrom glue glue
#' @import rlang
#' @importFrom units set_units drop_units
#' 
#' @return dataframe of outputs
#' @export 
#' 
#' @details
#' Example csv with all the spec_params & site_params required? 
#' - Note that the final growth dataframe is inclusive of the start and end date, so the environmental vectors must be the same
#'
#' @examples "see here" link?
#' @seealso [check_grow()]
grow_macroalgae <- function(start, grow_days, temperature, salinity, light, velocity, nitrate, ammonium, other_N = NA, ni_uptake, am_uptake, ot_uptake = NA, site_params, spec_params, initials, sparse_output = T, other_constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)) {
  
  t <- seq(lubridate::yday(start), lubridate::yday(start)+grow_days, 1)
  dates <- seq(start, start + lubridate::duration(grow_days, "days"), by = 'days')
  
  # Site parameters
  farmV <- unname(site_params['farmA'] * site_params['hc'])
  
  # Placeholder vectors
  u_c <- I_top <- conc_nitrate <- conc_ammonium <- Nf <- Ns <- B_dw.mg <- B_ww.mg <- hm <- lambda <- lambda_0 <- conc_other <- Q_int <- Q_rel <- T_lim <- S_lim <- Q_lim <- I_lim <- growth_rate <- Ns_to_Nf <- Ns_loss <- Nf_loss <- up_Am <- up_Ni <- up_Ot <- # red_Am <- remin <- 
    as.numeric(rep(NA, length.out = length(t)))

  add_ammonium     <- ammonium
  add_nitrate      <- nitrate
  
  # Which external factors are we using?
  use_Slim  <- ifelse(any(is.na(salinity)), yes = F, no = T)
  add_other <- ifelse(any(is.na(other_N)), yes = rep(0, length(t)), no = other_N)
  
  # External starting state
  conc_ammonium[1] <- add_ammonium[1]
  conc_nitrate[1]  <- add_nitrate[1]
  conc_other[1]    <- add_other[1]
  
  # Macroalgae starting state
  if (is.na(initials['Q_int'])) {
    initials['Q_int'] <- Q_int(Nf = initials['Nf'], Q_rel = initials['Q_rel'], spec_params = spec_params)
  }
  Nf[1]              <- unname(initials['Nf'])
  Ns[1]              <- unname(Nf[1]*(initials['Q_int']/spec_params['Q_min'] - 1))

  # Main run, after starting state
  for (i in 1:length(t)) {
    
    # Macroalgae state at start of day
    Q_int[i]       <- Q_int(Nf = Nf[i], Ns = Ns[i], spec_params = spec_params)
    Q_rel[i]       <- Q_rel(Q_int = Q_int[i], spec_params = spec_params)
    B_dw.mg[i]     <- unname(10^3 * (Nf[i]+Ns[i]) / Q_int[i])
    B_ww.mg[i]     <- unname(B_dw.mg[i] * spec_params['DWWW'])
    hm[i]          <- algae_height(Nf[i], spec_params)
    
    # Environmental state (incoming)
    u_c[i]         <- suppressWarnings(
                        u_c(U0 = velocity[i], 
                            macro_state = c(biomass = B_ww.mg[i]/1000, hm[i]),
                            site_params = site_params,
                            spec_params = spec_params,
                            constants = other_constants
                        ))
    U_0            <- set_units(set_units(velocity[i], "m s-1"), "m d-1")
    lambda[i]      <- (u_c[i] * U_0)/farmV 
    lambda_0[i]    <- U_0/farmV 
    
    # Nutrient delivery
    conc_ammonium[i]     <- add_ammonium[i] * lambda_0[i]/lambda[i]
    conc_nitrate[i]      <- add_nitrate[i]  * lambda_0[i]/lambda[i]
    conc_other[i]        <- add_other[i]    * lambda_0[i]/lambda[i]
    
    # Environmental limitation on growth
    T_lim[i]       <- T_lim(temperature[i], spec_params)
    Q_lim[i]       <- Q_lim(Nf[i], Ns[i], spec_params)
    I_top[i]       <- unname(light[i] * exp(-site_params['kW'] * site_params['d_top']))
    I_lim[i]       <- I_lim(Nf[i], I_top[i], spec_params, site_params)
    S_lim[i]       <- ifelse(use_Slim == T, yes = S_lim(salinity[i], spec_params), no = 1)
    
    # Biomass loss - CHECK THIS WORKS WHEN PARAMETERS ARE MISSING
    U_c            <- velocity[i] * u_c[i]
    D_m            <- suppressMessages(
      loss(U0 = U_c, turbulence = site_params['turbulence'], spec_params = spec_params)
    )
    
    growth_rate[i]  <- unname(spec_params['mu'] * I_lim[i] * T_lim[i] * S_lim[i] * Q_lim[i])
    
    # Nitrogen pool changes
    Ns_to_Nf[i]     <- pmin(growth_rate[i] * Ns[i], Ns[i]) # cannot convert more Ns than available
    Ns_loss[i]      <- unname(D_m * Ns[i])
    Nf_loss[i]      <- unname(D_m * Nf[i])

    up_Am[i]        <-  pmin(conc_ammonium[i],
                             (1 - Q_rel(Nf = Nf[i], Ns = Ns[i], spec_params = spec_params)) * (B_dw.mg[i]/1000) *
                               get_uptake(conc = conc_ammonium[i],
                                          uptake_shape = am_uptake,
                                          Nform_abbr = "am",
                                          spec_params = spec_params)
                             )
    
    up_Ni[i]        <-  pmin(conc_nitrate[i],
                             (1 - Q_rel(Nf = Nf[i], Ns = Ns[i], spec_params = spec_params)) * (B_dw.mg[i]/1000) * 
                               get_uptake(conc = conc_nitrate[i], 
                                          uptake_shape = ni_uptake, 
                                          Nform_abbr = "ni", 
                                          spec_params = spec_params)
                             )
    
    # If you're not at the final day
    if (i < length(t)) {
      # Changes in external state
      conc_ammonium[i+1] <- conc_ammonium[i] - up_Am[i] + Ns_loss[i]
      conc_nitrate[i+1] <- conc_nitrate[i] - up_Ni[i] 
      
      # Change in algae state
      Nf[i+1]      <- Nf[i] + Ns_to_Nf[i] - Nf_loss[i]
      Ns[i+1]      <- Ns[i] + up_Am[i] + up_Ni[i] - Ns_to_Nf[i] - Ns_loss[i]
      
      # IF MACROALGAE DIES
      if (Nf[i+1] <= 0) break
    }
  # End of main model run
  }
   
  # Put all the data together for outputs 
  if (sparse_output == F) {
    df <- data.frame(
      t = t, 
      date = dates,
      Nf = Nf,
      Ns = Ns,
      growth_rate = growth_rate,
      Ns_to_Nf = Ns_to_Nf,
      Ns_loss = Ns_loss,
      Nf_loss = Nf_loss,
      Q_int = Q_int,
      Q_rel = Q_rel,
      Q_lim = Q_lim,
      B_dw.mg = B_dw.mg,
      B_ww.mg = B_ww.mg,
      hm = hm,
      add_nitrate = nitrate,
      conc_nitrate = conc_nitrate,
      up_Ni = up_Ni,
      add_ammonium = ammonium,
      conc_ammonium = conc_ammonium,
      up_Am = up_Am,
      add_other = add_other,
      conc_other_N = other_N,
      up_Ot = up_Ot,
      temperature = temperature,
      T_lim = T_lim,
      salinity = salinity,
      S_lim = S_lim,
      light = light,
      I_top = I_top,
      I_lim = I_lim,
      velocity = velocity,
      u_c = u_c,
      lambda = lambda
    )
  } else {
    df <- data.frame(
      date = dates,
      Nf = Nf,
      Ns = Ns,
      growth_rate = growth_rate,
      Ns_to_Nf = Ns_to_Nf,
      Ns_loss = Ns_loss,
      Nf_loss = Nf_loss,
      Q_int = Q_int,
      Q_rel = Q_rel,
      Q_lim = Q_lim,
      B_dw.mg = B_dw.mg,
      B_ww.mg = B_ww.mg,
      hm = hm,
      conc_nitrate = conc_nitrate,
      up_Ni = up_Ni,
      conc_ammonium = conc_ammonium,
      up_Am = up_Am,
      up_Ot = up_Ot,
      T_lim = T_lim,
      S_lim = S_lim,
      I_top = I_top,
      I_lim = I_lim,
      u_c = u_c
    )
  }
  return(df)
}


