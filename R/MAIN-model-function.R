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
#' @param other_constants a named vector of miscellaneous constants, including:
#' * s = 0.0045, the experimentally-derived shape factor for macroalgae movement in water
#' * gam = 1.13, the modified drag coefficient for macroalgae
#' * a2 = 0.2^2, 
#' * Cb = 0.0025, the bottom friction coefficient
#'
#' @importFrom lubridate is.Date ymd duration yday parse_date_time
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @import dplyr
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
grow_macroalgae <- function(start, 
                            grow_days, 
                            temperature, 
                            salinity,
                            light, 
                            velocity, 
                            nitrate, 
                            ammonium, 
                            other_N, 
                            ni_uptake, 
                            am_uptake, 
                            # ot_uptake, 
                            site_params, 
                            spec_params, 
                            initials,
                            sparse_output = T,
                            other_constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)) {
  
  # Parse start date
  if (lubridate::is.Date(start)) {
    start_date <- start
  } else if (is.integer(start) | is.numeric(start)) {
    start_date <- tryCatch(
      expr = lubridate::parse_date_time(x = paste(2015, start), orders = c("yj", "m-d")),
      warning = function(cond) {
        rlang::inform("Provided 'start' value could not be coerced into a date! Here's the warning from lubridate::parse_date_time():")
        rlang::abort(message = conditionMessage(cond))
        }
      )
  } else if (is.character(start)) {
    start_date <- tryCatch(
      expr = lubridate::parse_date_time(x = start, orders = c("Y-m-d", "d-m-Y")),
      warning = function(cond) {
        rlang::inform("Provided 'start' value could not be coerced into a date! Here's the warning from lubridate::parse_date_time():")
        rlang::abort(message = conditionMessage(cond))
      }
    )
  } else {
    rlang::abort("'start' must be defined as a date, a character coercible to a date, or a day number")
  }

  # Populate start and end dates to create ts
  if (missing(grow_days)) {
    grow_days <- length(temperature)-1
  } else if (grow_days == length(temperature)) {
    grow_days <- grow_days-1
  }
  t <- seq(lubridate::yday(start_date), lubridate::yday(start_date)+grow_days, 1)
  dates <- seq(start_date, start_date + lubridate::duration(grow_days, "days"), by = 'days')
  
  # Light, temperature and nutrient levels are passed to the function as vectors - check length
  if (length(temperature) != length(t)) {
    rlang::abort(message = glue::glue(
      "Error: temperature vector has length {obs} but timespan vector has length {exp}",
      obs = length(temperature),
      exp = length(t)))
  }
  if (length(salinity) != length(t)) {
    rlang::abort(message = glue::glue(
      "Error: salinity vector has length {obs} but timespan vector has length {exp}",
      obs = length(salinity),
      exp = length(t)))
  }
  if (length(light) != length(t)) {
    rlang::abort(message = glue::glue(
      "Error: light vector has length {obs} but timespan vector has length {exp}",
      obs = length(light),
      exp = length(t)))
  }
  if (length(nitrate) != length(t)) {
    rlang::abort(message = glue::glue(
      "Error: nitrate vector has length {obs} but timespan vector has length {exp}",
      obs = length(nitrate),
      exp = length(t)))
  }
  if (length(ammonium) != length(t)) {
    rlang::abort(message = glue::glue(
      "Error: ammonium vector has length {obs} but timespan vector has length {exp}",
      obs = length(ammonium),
      exp = length(t)))
  }
  if (length(velocity) != length(t)) {
    rlang::abort(message = glue::glue(
      "Error: velocity vector has length {obs} but timespan vector has length {exp}",
      obs = length(velocity),
      exp = length(t)))
  }
  
  # Site parameters
  farmV <- unname(site_params['farmA'] * site_params['hc'])         # Volume of farm site
  
  # Placeholder vectors
  u_c <- I_top <- conc_nitrate <- conc_ammonium <- det <- Nf <- Ns <- N_int <- N_rel <- B_dw.mg <- B_ww.mg <- hm <- 
    lambda <- lambda_0 <- conc_other <- Q_int <- Q_rel <- T_lim <- S_lim <- Q_lim <- I_lim <- growth_rate <- 
    Ns_to_Nf <- Ns_loss <- Nf_loss <- up_Am <- up_Ni <- up_Ot <- # red_Am <- remin <- 
    as.numeric(rep(NA, length.out = length(t)))

  add_ammonium     <- ammonium
  add_nitrate      <- nitrate
  
  # For adding other_N (e.g. urea, amino acids) - currently does nothing
  if (!missing(other_N) | length(other_N) == 0 | is.na(other_N)) {
    add_other <- numeric(length = length(t))
  } else if (length(other_N) != length(t)) {
    rlang::abort(message = glue::glue("Error: other_N vector has length {obs} but timespan vector has length {exp}", obs = length(other_N), exp = length(t)))
  } else {
    add_other <- other_N
  }
  
  # External starting state
  conc_ammonium[1] <- add_ammonium[1]
  conc_nitrate[1]  <- add_nitrate[1]
  conc_other[1]    <- add_other[1]
  
  # Macroalgae starting state
  Nf[1]            <- unname(initials['Nf'])  # Fixed nitrogen
  Ns[1]            <- Nf[1]*(unname(initials['Q_int'])/spec_params['Q_min'] - 1)          # Stored nitrogen
  det[1]           <- 10
  
  # Main run, after starting state
  for (i in 1:length(t)) {
    
    # Macroalgae state at start of day
    Q_int[i]       <- Q_int(Nf[i], Ns[i], spec_params)
    Q_rel[i]       <- Q_rel(Q_int[i], spec_params)
    N_int[i]       <- N_int(Q_rel = Q_rel[i], Q_int = NA, spec_params)
    N_rel[i]       <- N_rel(N_int[i], spec_params)
    B_dw.mg[i]     <- (Nf[i]+Ns[i]) / N_int[i]
    B_ww.mg[i]     <- B_dw.mg[i] * unname(spec_params['DWWW'])
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
    S_lim[i]       <- S_lim(salinity[i], spec_params)
    Q_lim[i]       <- Q_lim(Nf[i], Ns[i], spec_params)
    I_top[i]       <- unname(light[i] * exp(-site_params['kW'] * site_params['d_top']))
    I_lim[i]       <- I_lim(Nf[i], I_top[i], spec_params, site_params)
    # PUT SALINITY LIMITATION HERE
    
    # Biomass loss - CHECK THIS WORKS WHEN PARAMETERS ARE MISSING
    U_c            <- velocity[i] * u_c[i]
    D_m            <- loss(U0 = U_c, turbulence = site_params['turbulence'], spec_params = spec_params)
    
    # Nitrogen pool changes
    growth_rate[i]  <- unname(spec_params['mu'] * I_lim[i] * T_lim[i] * S_lim[i] * Q_lim[i])
    Ns_to_Nf[i]     <- pmin(growth_rate[i] * Ns[i], Ns[i]) # cannot convert more Ns than available
    Ns_loss[i]      <- unname(D_m * Ns[i])
    Nf_loss[i]      <- unname(D_m * Nf[i])
    # red_Am[i]       <- unname(site_params['Rd'] * conc_ammonium[i]) # Reduction of ammonium (to nitrate)
    # remin[i]        <- unname(site_params['rL'] * det[i]) # Remineralisation of detritus (to ammonium)
    
    up_Am[i]        <-  pmin(
                          conc_ammonium[i],
                          Q_rel(Q_int(Nf[i], Ns[i], spec_params), spec_params) * (B_dw.mg[i]/1000) *
                            get_uptake(conc = conc_ammonium[i],
                                       uptake_shape = am_uptake,
                                       Nform_abbr = "am",
                                       spec_params = spec_params)
                        )
    
    up_Ni[i]        <-  pmin(
                          conc_nitrate[i],
                          Q_rel(Q_int(Nf[i], Ns[i], spec_params), spec_params) * (B_dw.mg[i]/1000) * 
                            get_uptake(conc = conc_nitrate[i], 
                                       uptake_shape = ni_uptake, 
                                       Nform_abbr = "ni", 
                                       spec_params = spec_params)
                        )
    
    # up_Ot[i]        <-  pmin(
    #                       conc_other[i],
    #                       Q_rel(Q_int(Nf[i], Ns[i], spec_params), spec_params) * (B_dw.mg[i]/1000) * 
    #                         get_uptake(conc = conc_other[i], 
    #                                    uptake_shape = ot_uptake, 
    #                                    Nform_abbr = "ot", 
    #                                    spec_params = spec_params)
    #                     )
    
    # If you're not at the final day
    if (i < length(t)) {
      # Changes in external state
      conc_ammonium[i+1] <- conc_ammonium[i] - up_Am[i] + Ns_loss[i] #- red_Am[i] + remin[i]
      conc_nitrate[i+1] <- conc_nitrate[i] - up_Ni[i] #+ red_Am[i]
      det[i+1]     <- det[i] + Nf_loss[i] #- remin[i]
      
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
      N_int = N_int,
      N_rel = N_rel,
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
      lambda = lambda,
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
      N_int = N_int,
      N_rel = N_rel,
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


