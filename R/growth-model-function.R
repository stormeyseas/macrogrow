#' Grow cultured macroalgae
#'
#' @param start_date A starting (out-planting) day for macroalgae growth. Can be given as a date class object from `as.Date`, a character string coercible to date by `lubridate::ymd()` (e.g. "2009-03-08"), or an integer representing a day of the year.
#' @param grow_days An integer of the number of days to grow macroalgae before harvesting.
#' @param temperature A vector of temperature readings
#' @param light 
#' @param refreshrate 
#' @param nitrate 
#' @param ammonium 
#' @param other_N possibility to add urea?
#' @param site_params 
#' @param spec_params 
#' @param Nf_start 
#' @param Q_start 
#'
#' @importFrom lubridate ymd time_length yday is.Date duration
#' @importFrom magrittr use_pipe
#' 
#' @return a list of dataframes describing internal and external states at each timestep
#' @export
#'
#'@details 
#'
grow_macroalgae <- function(start, grow_days, temperature, light, velocity, nitrate, ammonium, other_N, # model does not yet have parameters for uptake rate of other_N
                            site_params, spec_params, Nf_start, Q_start) {

  # Get start and end dates
  if (is.Date(start)) {
    start_date <- start
  } else if (is.integer(start) | is.numeric(start)) {
    if (missing(start_year)) {
      start_year = 2000
    }
    start_date <- (parse_date_time(x = paste(start_year, start), orders = "yj")) # - duration(1, "days")
  }
  end_date <- start_date + duration(grow_days, "days")
  start_t <- yday(start_date)
  end_t <- start_t + grow_days
  
  # Set up base outputs dataframe
  outputs <- data.frame(t = seq(start_t, end_t, 1), date = seq(start_date, end_date, by = 'days'))
  
  # Light, temperature and nutrient levels must be passed to the function as vectors - check length
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
    mutate(TT = temperature, I = light, I_top = NA, Ni_base = nitrate, Am_base = ammonium, Ni_add = NA, Am_add = NA, det = 10, # CHANGE THIS?
           U = velocity, u_c = 1, lambda = NA)
  if (!missing(other_N)) {
    externals$other_N <- other_N
  }
  
  # Set up results data frames
  internals <- outputs %>%
    mutate(Nf = NA, Ns = NA, Q_lim = NA, T_lim = NA, I_lim = NA, N_perc = NA, B_dw.mg = NA, B_ww.mg = NA, hm = NA)
  
  rates <- outputs %>%
    mutate(up_Am = NA, up_Ni = NA, growth_rate = NA, Ns_to_Nf = NA, Nf_loss = NA, N_change = NA)

  # Miscellaneous constants
  rL <- 0.2
  Rd <- 0.1
  kW <- 0.58 #0.65
  
  # Set up site parameters & starting conditions
  d_top <- site_params$value[site_params$parameter == "d_top"]
  hc <- site_params$value[site_params$parameter == "hc"]
  farmV <- site_params$value[site_params$parameter == "farmA"] * hc
  hz <- site_params$value[site_params$parameter == "hz"]
  externals$lambda[1] <- lambda <- (externals$U[1]*60*60*24)/farmV # U needs to be in m d-1
  externals$Am_add[1] <- externals$Am_base[1]
  externals$Ni_add[1] <- externals$Ni_base[1]
  
  # Set up all species parameters & starting conditions
  V_am <- spec_params$value[spec_params$parameter == "V_am"]
  K_am <- spec_params$value[spec_params$parameter == "K_am"]
  V_ni <- spec_params$value[spec_params$parameter == "V_ni"]
  K_ni <- spec_params$value[spec_params$parameter == "K_ni"]
  Q_min <- spec_params$value[spec_params$parameter == "Q_min"]
  Q_max <- spec_params$value[spec_params$parameter == "Q_max"]
  K_c <- spec_params$value[spec_params$parameter == "K_c"]
  mu  <- spec_params$value[spec_params$parameter == "mu"]
  N_min <- spec_params$value[spec_params$parameter == "N_min"]
  N_max <- spec_params$value[spec_params$parameter == "N_max"]
  D_m  <- spec_params$value[spec_params$parameter == "D_m"]
  a_cs <- spec_params$value[spec_params$parameter == "a_cs"]
  I_o <- spec_params$value[spec_params$parameter == "I_o"]
  T_opt <- spec_params$value[spec_params$parameter == "T_opt"]
  T_min <- spec_params$value[spec_params$parameter == "T_min"]
  T_max <- spec_params$value[spec_params$parameter == "T_max"]
  h_a <- spec_params$value[spec_params$parameter == "h_a"]
  h_b <- spec_params$value[spec_params$parameter == "h_b"]
  h_c <- spec_params$value[spec_params$parameter == "h_c"]
  DWWW <- spec_params$value[spec_params$parameter == "DWWW"]
  
  internals$Nf[1] <- Nf <- Nf_start
  Q <- Q_start
  internals$Ns[1] <- Ns <- Nf*(Q/Q_min - 1)

    # Start model run
    for (i in 1:nrow(outputs)) {
      
      # Start of the day
      internals$N_perc[i] <- N_perc <- N_max - Q*(N_max - N_min)
      internals$B_dw.mg[i] <- B_dw.mg <- (Nf+Ns) / N_perc
      internals$B_ww.mg[i] <- B_ww.mg <- B_dw.mg * DWWW
      internals$hm[i] <- hm <- algae_height(Nf, h_a, h_b, h_c)
      
      # Temperature limitation on growth
      TT <- externals$TT[i]
      internals$T_lim[i] <- T_lim <- T_lim(TT, T_max, T_opt, T_min)
      
      # Internal nutrient limitation on growth
      internals$Q_lim[i] <- Q_lim <- Q_lim(Nf, Ns, Q_min, Q_max, K_c)
      
      # Light limitation on growth
      I <- externals$I[i]
      internals$I_lim[i] <- I_lim <- I_lim(Nf, I_o, a_cs, hm, d_top, I, kW)
      
      # Environmental additions
      Am_base <- externals$Am_base[i]
      Ni_base <- externals$Ni_base[i]
      det <- externals$det[i]
      
      # Environmental state (incoming)
      U <- externals$U[i]
      externals$uc[i] <- uc <- attenuate(U, hm, hz, hc, d_top, biomass = B_ww.mg/1000)
      externals$lambda[i] <- lambda <- uc*(U*60*60*24)/farmV 

      # Nitrogen pool changes
      rates$growth_rate[i] <- growth_rate <- max(mu * I_lim * T_lim * Q_lim, 0)
      rates$Ns_to_Nf[i] <- Ns_to_Nf <- max(growth_rate * Ns, 0)
      Ns_loss <- max(D_m * Ns, 0)
      rates$Nf_loss[i] <- Nf_loss <- max(D_m * Nf, 0)
      rates$up_Am[i] <- up_Am <- max(N_uptake(conc = Am_base, Q, biomass = B_dw.mg/1000, V = V_am, K = K_am), 0)
      rates$up_Ni[i] <- up_Ni <- max(N_uptake(conc = Ni_base, Q, biomass = B_dw.mg/1000, V = V_ni, K = K_ni), 0)
      red_Am <- Rd * Am_base
      remin <- rL * det
      
      externals$Am_add[i] <- Am_add <- lambda * Am_base
      externals$Ni_add[i] <- Ni_add <- lambda * Ni_base
      
      if (i != nrow(outputs)) {
        # New environmental states for tomorrow (cannot go below 0)
        externals$Am_base[i+1] <- max(Am_base + Am_add - up_Am + Ns_loss + remin - red_Am, 0)
        externals$Ni_base[i+1] <- max(Ni_base + Ni_add - up_Ni + red_Am, 0)
        externals$det[i+1] <- max(det + Nf_loss - remin, 0)
        
        # Algae starting state for tomorrow
        internals$Nf[i+1] <- Nf <- Nf + Ns_to_Nf - Nf_loss
        internals$Ns[i+1] <- Ns <- Ns + up_Am + up_Ni - Ns_to_Nf - Ns_loss
        Q <- Q_min*(Ns/Nf + 1)
        internals$N_perc[i+1] <- N_perc <- N_max - Q*(N_max - N_min)
        rates$N_change[i+1] <- up_Am + up_Ni - Ns_loss - Nf_loss
      }
    }
    return(cbind(externals, internals, rates))
  }
