#' Grow macroalgae
#' 
#' @description
#' Initiate and grow macroalgae.
#'
#' @param start integer, start of the growth period (day of at-sea deployment). Defaults to 1.
#' @param grow_days integer, number of day in growing period - if missing will take the length of the temperature vector
#' @param temperature a vector of daily temperatures (\eqn{^{\circ}}C)
#' @param salinity a vector of daily salt concentrations (g L\eqn{^{-1}})
#' @param light a vector of surface light (umol m\eqn{^{-2}} s\eqn{^{-1}})
#' @param kW a vector of light attenuation coefficients for open water (m\eqn{^{-1}})
#' @param velocity a vector of water velocities (m s\eqn{^{-1}})
#' @param nitrate a vector of nitrate concentrations (mg m\eqn{^{-3}})
#' @param ammonium a vector of ammonium concentrations (mg m\eqn{^{-3}})
#' @param ni_uptake shape for nitrate uptake, either "MM" for Michaelis-Menton saturation kinetics or "linear" for linear uptake
#' @param am_uptake shape for ammonium uptake, either "MM" for Michaelis-Menton saturation kinetics or "linear" for linear uptake
#' @param site_params a named vector of species-specific parameters - see details
#' @param spec_params a named vector of site-specific parameters - see details
#' @param initials a named vector of the macroalgae starting conditions
#' @param sparse_output logical, whether to include input vectors and other non-essential information in final dataframe (default = TRUE)
#' @param other_constants a named vector of other constants for intermediate equations (see `u_c()`)
#'
#' @importFrom glue glue
#' @import rlang
#' @importFrom units set_units drop_units
#' 
#' @return matrix of outputs
#' @export 
#' 
#' @details
#' Example csv with all the spec_params & site_params required? 
#' - Note that the final growth dataframe is inclusive of the start and end date, so the environmental vectors must be the same
#'
#' @examples "see here" link?
#' @seealso [check_grow()]
grow_macroalgae <- function(
  t = 1:30,
  temperature,
  salinity,
  light,
  kW,
  velocity,
  nitrate,
  ammonium,
  ni_uptake = NA,
  am_uptake = NA,
  site_params,
  spec_params,
  initials,
  sparse_output = T,
  other_constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)
) {
  
  # Placeholder vectors
  u_c <- I_top <- conc_nitrate <- conc_ammonium <- Nf <- Ns <- B_dw.mg <- B_ww.mg <- hm <- lambda <- lambda_0 <- Q_int <- Q_rel <- T_lim <- S_lim <- Q_lim <- I_lim <- growth_rate <- Ns_to_Nf <- Ns_loss <- Nf_loss <- up_Am <- up_Ni <- 
    # red_Am <- remin <- 
    as.numeric(rep(NA, length(t)))

  add_ammonium     <- ammonium
  add_nitrate      <- nitrate

  use_Slim  <- ifelse(any(is.na(salinity)), yes = F, no = T)
  use_Ilim  <- ifelse(any(is.na(c(light, kW))), yes = F, no = T)
  use_Uc    <- ifelse(any(is.na(velocity)), yes = F, no = T)

  # External starting state
  conc_ammonium[1] <- add_ammonium[1]
  conc_nitrate[1]  <- add_nitrate[1]
  
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
    B_dw.mg[i]     <- Nf_to_biomass(Nf = Nf[i], Ns = Ns[i], Q_rel = Q_rel[i], dry = T, spec_params = spec_params)
    B_ww.mg[i]     <- Nf_to_biomass(Nf = Nf[i], Ns = Ns[i], Q_rel = Q_rel[i], dry = F, spec_params = spec_params)
    hm[i]          <- height(Nf[i], spec_params)
    
    # Environmental state (incoming)
    if (use_Uc) {
      biom <- B_ww.mg[i] |> units::set_units("mg") |> units::set_units("g") |> units::drop_units()
      u_c[i] <- u_c(
        U0 = velocity[i], # m/s
        macro_state = c(biomass = biom, hm = hm[i]),
        site_params = site_params,
        spec_params = spec_params,
        constants = other_constants
      )
      U_0 <- velocity[i] |> units::set_units("m s-1") |> units::set_units("m d-1") |> units::drop_units() # This is now m/d-1
      lambda[i]      <- (u_c[i] * U_0)/unname(site_params['farmA'] * site_params['hc']) 
      lambda_0[i]    <- U_0/unname(site_params['farmA'] * site_params['hc']) 
    } else {
      u_c[i] <- lambda[i] <- lambda_0[i] <- 1
    }
    
    # Nutrient delivery
    conc_ammonium[i]     <- add_ammonium[i] * lambda_0[i]/lambda[i]
    conc_nitrate[i]      <- add_nitrate[i]  * lambda_0[i]/lambda[i]
    
    # Nitrogen uptake
    up_Am[i]        <- min(
      # Concentration of ammonium available
      conc_ammonium[i],
      # Uptake rate in current state
      (1 - Q_rel[i]) * B_dw.mg[i]/1000 *
        suppressMessages(get_uptake(
          conc = conc_ammonium[i], 
          spec_params = spec_params,
          uptake_shape = am_uptake, 
          Nform_abbr = "am"))
    )
    
    up_Ni[i]        <- min(
      conc_nitrate[i],
      (1 - Q_rel[i]) * (B_dw.mg[i]/1000) * 
        suppressMessages(get_uptake(
          conc = conc_nitrate[i], 
          uptake_shape = ni_uptake, 
          Nform_abbr = "ni", 
          spec_params = spec_params))
    )
    
    # Environmental limitation on growth
    T_lim[i]       <- T_lim(Tc = temperature[i], spec_params = spec_params)
    I_top[i]       <- unname(light[i] * exp(-kW[i] * site_params['d_top']))
    I_lim[i]       <- ifelse(use_Ilim, I_lim(Nf[i], I_top[i], kW[i], spec_params, site_params), 1)
    S_lim[i]       <- ifelse(use_Slim, S_lim(salinity[i], spec_params), 1)
    Q_lim[i]       <- Q_lim(Nf[i], Ns[i], spec_params)
    growth_rate[i] <- unname(spec_params['mu'] * min(T_lim[i], I_lim[i], S_lim[i]) * Q_lim[i])
    
    # Nitrogen fixation
    Ns_to_Nf[i]     <- min(growth_rate[i] * Ns[i], Ns[i])
    
    # Biomass loss
    U_c            <- velocity[i] * u_c[i] # m/s
    D_m            <- loss(U0 = U_c, spec_params = spec_params)
    N_loss         <- biomass_to_Nf(biomass = (B_ww.mg[i] * D_m), Q_rel = Q_rel[i], spec_params = spec_params, dry = F)
    Ns_loss[i]     <- unname(N_loss["Nf"])
    Nf_loss[i]     <- unname(N_loss["Ns"])
    
    # If you're not at the final day, set up for next day
    if (i < length(t)) {
      # IF MACROALGAE DIES
      if (Nf[i] <= 0) break
      
      # Change in algae state
      Nf[i+1]      <- Nf[i] + Ns_to_Nf[i] - Nf_loss[i]
      Ns[i+1]      <- Ns[i] + up_Am[i] + up_Ni[i] - Ns_to_Nf[i] - Ns_loss[i]
    }
  # End of main model run
  }
  
  # Some quick renaming
  add_nitrate <- nitrate
  add_ammonium <- ammonium
  
  # Put all the data together for outputs 
  if (sparse_output == F) {
    df <- cbind(t, Nf, Ns, growth_rate, Ns_to_Nf, Ns_loss, Nf_loss, Q_int, Q_rel, Q_lim, B_dw.mg, B_ww.mg, hm, add_nitrate, conc_nitrate, up_Ni, add_ammonium, conc_ammonium, up_Am, temperature, T_lim, salinity, S_lim, light, I_top, I_lim, velocity, u_c, lambda)
  } else {
    df <- cbind(t, Nf, Ns, growth_rate, Ns_to_Nf, Ns_loss, Nf_loss, Q_int, Q_rel, Q_lim, B_dw.mg, B_ww.mg, hm, conc_nitrate, up_Ni, conc_ammonium, up_Am, T_lim, S_lim, I_top, I_lim, u_c)
  }
  return(df)
}


