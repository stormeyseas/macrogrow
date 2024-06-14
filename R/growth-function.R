grow_macroalgae <- function(start, grow_days, 
                            temperature, light, velocity, 
                            nitrate, ammonium, other_N, # model does not yet have parameters for uptake rate of other_N
                            site_params, spec_params, 
                            other_constants = c(rL = 0.2, Rd = 0.1),
                            initials) {

  # Parse start date
  if (lubridate::is.Date(start)) {
    start_date <- start
  } else if (is.integer(start) | is.numeric(start)) {
    if (missing(start_year)) {start_year = 2000}
    start_date <- (parse_date_time(x = paste(start_year, start), orders = "yj")) # - duration(1, "days")
  } else if (is.character(start)) {
    start_date <- lubridate::ymd(start)
  }
  
  # Populate start and end dates and ts
  end_date <- start_date + lubridate::duration(grow_days, "days")
  start_t <- lubridate::yday(start_date)
  end_t <- start_t + grow_days
  
  # Set up base outputs dataframe
  outputs <- data.frame(t = seq(start_t, end_t, 1), date = seq(start_date, end_date, by = 'days'))
  
  # Light, temperature and nutrient levels are passed to the function as vectors - check length
  if (length(temperature) != nrow(outputs)) {
    stop("Error: temperature vector is length ", length(temperature), " but timespan vector is length ", nrow(outputs))
  }
  if (length(light) != nrow(outputs)) {
    stop("Error: light vector is length ", length(light), " but timespan vector is length ", nrow(outputs))
  }
  if (length(nitrate) != nrow(outputs)) {
    stop("Error: nitrate vector is length ", length(nitrate), " but timespan vector is length ", nrow(outputs))
  }
  if (length(ammonium) != nrow(outputs)) {
    stop("Error: ammonium vector is length ", length(ammonium), " but timespan vector is length ", nrow(outputs))
  }
  if (length(velocity) != nrow(outputs)) {
    stop("Error: velocity vector is length ", length(velocity), " but timespan vector is length ", nrow(outputs))
  }
  
  # Add environmental vectors to externals dataframe
  externals <- outputs %>%
    dplyr::mutate(TT = temperature, 
           I = light, 
           I_top = NA, 
           Ni_conc = nitrate, 
           Am_conc = ammonium, 
           Ni_add = NA, 
           Am_add = NA, 
           det = 10, # CHANGE THIS?
           U = velocity, 
           u_c = 1, 
           lambda = NA)
  
  # Set up results data frames
  internals <- outputs %>%
    dplyr::mutate(Nf = NA, 
           Ns = NA, 
           Q_lim = NA, 
           T_lim = NA, 
           I_lim = NA, 
           N_int = NA, 
           B_dw.mg = NA, 
           B_ww.mg = NA, 
           hm = NA)
  
  rates <- outputs %>%
    dplyr::mutate(up_Am = NA, 
                  up_Ni = NA, 
                  growth_rate = NA, 
                  Ns_to_Nf = NA, 
                  Nf_loss = NA, 
                  N_change = NA)
  
  # For adding other_N (e.g. urea)
  # if (!missing(other_N)) {
    # if (length(other_N) != nrow(outputs)) {
    #   stop("Error: other_N vector is length ", length(other_N), " but timespan vector is length ", nrow(outputs))
    # }
  #   externals$other_N <- other_N
  # }

  # Site parameters
  farmV <- site_params['farmA'] * site_params['hc']                 # Volume of farm site
  externals$lambda[1] <- lambda <- (externals$U[1]*60*60*24)/farmV  # Refresh rate based on U (U in m d-1)
  externals$Am_add[1] <- externals$Am_conc[1]                       # Ambient ammonium concentration (no seaweed)
  externals$Ni_add[1] <- externals$Ni_conc[1]                       # Ambient nitrate concentration (no seaweed)
  
  # Starting state
  internals$Nf[1] <- Nf <- Nf <- initials['Nf']                     # Fixed nitrogen
  Q <- initials['Q']                                                # Internal nutrient quotient
  internals$Ns[1] <- Ns <- Nf*(Q/spec_params['Q_min'] - 1)          # Stored nitrogen

    # Start model run
    for (i in 1:nrow(outputs)) {
      
      # Start of the day
      internals$N_int[i]    <- N_int <- N_int(Q, spec_params)
      internals$B_dw.mg[i]  <- B_dw.mg <- (Nf+Ns) / N_int
      internals$B_ww.mg[i]  <- B_ww.mg <- B_dw.mg * spec_params['DWWW']
      internals$hm[i]       <- hm <- algae_height(Nf, spec_params)
      
      # Temperature limitation on growth
      TT <- externals$TT[i]
      internals$T_lim[i]    <- T_lim        <- T_lim(TT, spec_params)
      
      # Internal nutrient limitation on growth
      internals$Q_lim[i]    <- Q_lim        <- Q_lim(Nf, Ns, spec_params)
      
      # Light limitation on growth
      I <- externals$I[i]
      externals$I_top[i]    <- I_top        <- I * exp(-site_params['kW'] * site_params['d_top'])
      internals$I_lim[i]    <- I_lim        <- I_lim(Nf, I_top, spec_params, site_params)
      
      # Environmental additions
      Am_conc <- externals$Am_conc[i]
      Ni_conc <- externals$Ni_conc[i]
      det <- externals$det[i]
      
      # Environmental state (incoming)
      U <- externals$U[i]
      externals$u_c[i]      <- u_c          <- suppressWarnings(
                                                   u_c(U0 = U, macro_state = c(biomass = unname(B_ww.mg)/1000, hm = unname(hm)), 
                                                   SA_WW = 0.5 * (0.0306/2), site_params, 
                                                   constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025))
                                                   )
      externals$lambda[i]   <- lambda       <- u_c*(U*60*60*24)/farmV 

      # Nitrogen pool changes
      rates$growth_rate[i]  <- growth_rate  <- spec_params['mu'] * I_lim * T_lim * Q_lim
      rates$Ns_to_Nf[i]     <- Ns_to_Nf     <- min(growth_rate * Ns, Ns) # cannot convert more Ns than available
                               Ns_loss      <- spec_params['D_m'] * Ns
      rates$Nf_loss[i]      <- Nf_loss      <- spec_params['D_m'] * Nf
      rates$up_Am[i]        <- up_Am        <- MM_uptake(conc = Am_conc, V = spec_params['V_am'], K = spec_params['K_am']) * Q * B_dw.mg/1000
      rates$up_Ni[i]        <- up_Ni        <- MM_uptake(conc = Ni_conc, V = spec_params['V_ni'], K = spec_params['K_ni']) * Q * B_dw.mg/1000
                               red_Am       <- other_constants['Rd'] * Am_conc # Reduction of ammonium (to nitrate)
                               remin        <- other_constants['rL'] * det # Remineralisation of detritus (to ammonium)
      
      # New ammonium and nitrate delivered to site
      externals$Am_add[i]   <- Am_add       <- lambda * Am_conc
      externals$Ni_add[i]   <- Ni_add       <- lambda * Ni_conc
      
      # If you haven't reached the end of the run yet, set up for tomorrow
      if (i != nrow(outputs)) {
        # New environmental states cannot go below 0
        externals$Am_conc[i+1]              <- Am_conc + Am_add - up_Am + Ns_loss - red_Am + remin
        externals$Ni_conc[i+1]              <- Ni_conc + Ni_add - up_Ni           + red_Am
        externals$det[i+1]                  <- det                      + Nf_loss          - remin
        
        # Algae starting state
        internals$Nf[i+1]   <- Nf           <- Nf + Ns_to_Nf - Nf_loss
        internals$Ns[i+1]   <- Ns           <- Ns + up_Am + up_Ni - Ns_to_Nf - Ns_loss
                               Q            <- spec_params['Q_min']*(Ns/Nf + 1)
        internals$N_int[i+1] <- N_int       <- N_int(Q, spec_params)
        rates$N_change[i+1]                 <- up_Am + up_Ni - Ns_loss - Nf_loss
      }
    }
    
    df <- merge(externals, internals, by = c("t", "date"))
    df <- merge(df, rates, by = c("t", "date"))
    return(df)
  }

#' Macroalgae height
#'
#' @param Nf 
#' @param spec_params A vector of named numbers. Must include the parameters \eqn{h_a}, \eqn{h_b} and \eqn{h_c}
#'
#' @return
#'
#' @examples
algae_height <- function(Nf, spec_params) {
  hm <- (Nf/spec_params['h_a'])^spec_params['h_b'] + spec_params['h_c']
}

#' Light limitation on growth
#'
#' @param Nf 
#' @param I 
#' @param spec_params description
#' @param site_params description
#'
#' @return
#' @export
#'
#' @examples
I_lim <- function(Nf, I_top, spec_params, site_params) {
  hm <- algae_height(Nf, spec_params)
  k_ma <-  Nf * spec_params['a_cs'] * max(hm / site_params['d_top'], 1) * 1 / (min(hm, site_params['d_top']))
  
  x <- exp(I_top / spec_params['I_o'])
  K <- site_params['kW'] + k_ma
  
  Ilim <- (exp(1) / (K * site_params['d_top'])) * ((1 / x) ^ (1 / exp(K * site_params['d_top'])) - 1 / x)
  #Ilim <- exp(-(I/spec_params['I_top'] * exp(-dw) * exp(-cw))) - exp(-(I/spec_params['I_top'] * exp(-dw)))
  
  return(Ilim)
}

#' Temperature limitation on growth
#' 
#' Given species parameters returns the relative limitation on growth rate according to a CTMI curve.
#'
#' @param TT temperature to evaluate
#' @param spec_params
#' @return A dataframe with relative temperature limitation (between 0 and 1) at each timestep.
#' @export
#'
#' @examples
#' 
T_lim <- function(TT, spec_params){
  
  if (spec_params['T_opt'] < spec_params['T_min']) {stop("Error: minimum temperature is higher than optimum temperature")}
  if (spec_params['T_opt'] > spec_params['T_max']) {stop("Error: maximum temperature is lower than optimum temperature")} 
  if (spec_params['T_opt']-spec_params['T_min'] <= spec_params['T_max']-spec_params['T_opt']) {stop("Species CTMI function not valid! Must satisfy T_opt-T_min > T_max-T_opt")}
  
  if (TT > spec_params['T_max']) {Tlim <- 0
  } else if (TT < spec_params['T_min']) {Tlim <- 0
  } else {Tlim <- ((TT - spec_params['T_max'])*(TT - spec_params['T_min'])^2)/((spec_params['T_opt'] - spec_params['T_min'])*((spec_params['T_opt'] - spec_params['T_min'])*(TT - spec_params['T_opt']) - (spec_params['T_opt'] - spec_params['T_max'])*(spec_params['T_opt'] + spec_params['T_min'] - 2*TT)))}
  
  return(Tlim)
}

#' Relative water attenuation within canopy
#' 
#' #' @description
#' A short description...
#'
#' @param U0 incoming incident water velocity (m/s)
#' @param macro_state vector of named numbers in the form c(biomass, hm) defining macroalgae wet weight (g) and height (m)
#' @param SA_WW conversion of wet weight to surface area (default is 0.5*0.5*0.0306 for \textit{Macrocystis pyrifera})
#' @param site_params vector of named numbers in the form c(hz, hc, site_params['d_top']) defining total water depth (m), vertical water column occupied by the canopy (m), and depth of the top of the canopy beneath the water surface (m)
#' @param constants vector of named numbers in the form c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025) defining extra constants for the attenuation submodel
#'
#' @return the relative water attenuation coefficient (u_c)
#' 
#'
#' @examples
u_c <- function(U0, 
                macro_state = c(biomass, hm), 
                SA_WW = 0.5 * (0.0306/2), site_params, 
                constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)){
  
  D <- SA_WW * min(macro_state['hm']/site_params['hc'], 1) * macro_state['biomass']
  Kd <- 0.5 * site_params['hz'] * D * constants['s'] * U0^(constants['gam'] - 2)
  Hc <- (site_params['d_top'] + site_params['hc']) / site_params['hz']
  
  drag_test <- sqrt(Kd * (1 - Hc) * Hc * (constants['Cb'] * Hc + constants['a2']) - constants['a2'] * constants['Cb'] * Hc)
  if (is.na(drag_test)) {
    u_c <- 1
  } else {
    u_c <- (-constants['a2'] - constants['Cb'] * Hc ^ 2 + (1 - Hc) * drag_test) / (Kd * Hc * (1 - Hc) ^ 3 - constants['a2'] - constants['Cb'] * Hc ^ 3)
  }
  return(u_c)
}

#' Relative water attenuation beneath canopy
#'
#' @return
#' @export
#' 
#' @inheritParams u_c
#'
#' @examples
u_b <- function(U0, macro_state = c(biomass, hm), SA_WW = 0.5 * (0.0306/2), site_params, constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)){
  uc <- uc(U0, macro_state, SA_WW, site_params, constants)
  Hc <- (site_params['d_top'] + site_params['hc']) / site_params['hz']
  u_b <- (1 - u_c * Hc) / (1 - Hc)
  return(u_b)
}

#' Total drag coefficient
#'
#' @return The total drag coefficient C_t
#' @export
#' 
#' @inheritParams u_c
#' 
C_t <- function(u_c, u_b, macro_state = c(biomass, hm), SA_WW = 0.5 * (0.0306/2), site_params, constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)) {
  D <- SA_WW * min(macro_state['hm']/site_params['hc'], 1) * macro_state['biomass']
  Kd <- 0.5 * site_params['hz'] * D * constants['s'] * U0^(constants['gam'] - 2)
  Hc <- (site_params['d_top'] + site_params['hc']) / site_params['hz']
  C_t <- (Kd * Hc * u_c ^ 2 + constants['Cb'] * u_b^2)
}

