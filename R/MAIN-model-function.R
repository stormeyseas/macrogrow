#' Grow macroalgae
#' 
#' @description
#' The main deal 
#'
#' @param start 
#' @param grow_days 
#' @param temperature 
#' @param light 
#' @param velocity 
#' @param nitrate 
#' @param ammonium 
#' @param other_N 
#' @param uptake_nitrate 
#' @param uptake_ammonium 
#' @param uptake_other 
#' @param site_params 
#' @param spec_params 
#' @param other_constants 
#' @param initials 
#'
#' @importFrom lubridate is.Date ymd duration yday
#' @importFrom glue glue
#' @import magrittr
#' @import dplyr
#' @import rlang
#' @importFrom units set_units
#' 
#' @return dataframe of outputs
#' @export 
#'
#' @examples "see here" link?
grow_macroalgae <- function(start, grow_days, temperature, light, velocity, nitrate, ammonium, other_N, # model does not yet have parameters for uptake rate of other_N
                            uptake_nitrate = "MM", uptake_ammonium = "MM", uptake_other = "MM", site_params, spec_params, other_constants = c(rL = 0.2, Rd = 0.1), initials) {

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
  
  # For adding other_N (e.g. urea, amino acids)
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
  Q_int <- unname(initials['Q_int'] )                                               # Internal nutrient quotient
  internals$Ns[1] <- Ns <- unname(Nf*(Q_int/spec_params['Q_min'] - 1))          # Stored nitrogen

    # Start model run
    for (i in 1:nrow(outputs)) {
      
      # Start of the day
      internals$N_int[i]    <- N_int(Q_int, NA, spec_params) # Takes Q_int or Q_rel
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
      externals$lambda[i]   <- lambda       <- (u_c*U*60*60*24)/farmV 

      # Nitrogen pool changes
      rates$growth_rate[i]  <- growth_rate  <- spec_params['mu'] * I_lim * T_lim * Q_lim
      rates$Ns_to_Nf[i]     <- Ns_to_Nf     <- min(growth_rate * Ns, Ns) # cannot convert more Ns than available
                               Ns_loss      <- spec_params['D_m'] * Ns
      rates$Nf_loss[i]      <- Nf_loss      <- spec_params['D_m'] * Nf
      red_Am                                <- other_constants['Rd'] * Am_conc # Reduction of ammonium (to nitrate)
      remin                                 <- other_constants['rL'] * det # Remineralisation of detritus (to ammonium)
      
      # Apply the correct uptake rate curve depending on parameters supplied
      rates$up_Am[i]        <- up_Am        <- 
        if(is.na(spec_params['M_am']) & is.na(spec_params['C_am'])) {
          MM_uptake(conc = Am_conc, V = spec_params['V_am'], K = spec_params['K_am']) * Q * B_dw.mg/1000
        } else if (is.na(spec_params['V_am']) & is.na(spec_params['K_am'])) {
          lin_uptake(conc = Am_conc, M = spec_params['M_am'], C = spec_params['C_am']) * Q * B_dw.mg/1000
        } else if (uptake_ammonium == "MM") {
          stop("Error: parameters for ammonium uptake missing! Michaelis-Menton uptake needs V_am and K_am")
        } else if (uptake_ammonium == "linear") {
          stop("Error: parameters for ammonium uptake missing! Linear uptake needs M_am and C_am")
        } else {
          stop("Error: Unknown uptake of ammonium. Specify uptake or provide parameters.")
        }
      
      rates$up_Ni[i]        <- up_Ni        <- 
        if(is.na(spec_params['M_am']) & is.na(spec_params['C_am'])) {
          MM_uptake(conc = Ni_conc, V = spec_params['V_ni'], K = spec_params['K_ni']) * Q * B_dw.mg/1000
        } else if (is.na(spec_params['V_ni']) & is.na(spec_params['K_ni'])) {
          lin_uptake(conc = Ni_conc, M = spec_params['M_ni'], C = spec_params['C_ni']) * Q * B_dw.mg/1000
        } else if (uptake_nitrate == "MM") {
          stop("Error: parameters for nitrate uptake missing! Michaelis-Menton uptake needs V_am and K_am")
        } else if (uptake_nitrate == "linear") {
          stop("Error: parameters for nitrate uptake missing! Linear uptake needs M_am and C_am")
        } else {
          stop("Error: Unknown uptake of nitrate. Specify uptake or provide parameters.")
        }
      
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
                              Q_int         <- unname(spec_params['Q_min'])*(Ns/Nf + 1)
        internals$N_int[i+1]  <- N_int      <- N_int(Q_int, NA, spec_params)
        rates$N_change[i+1]                 <- up_Am + up_Ni - Ns_loss - Nf_loss
      }
    }
    
    df <- merge(externals, internals, by = c("t", "date"))
    df <- merge(df, rates, by = c("t", "date"))
    return(df)
  }

