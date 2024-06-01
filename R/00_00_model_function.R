#' Title
#'
#' @param start_date A starting (out-planting) day for macroalgae growth. Can be given as a date class object from `as.Date`, a character string coercible to date by `lubridate::ymd()` (e.g. "2009-03-08"), or an integer representing a day of the year.
#' @param grow_days An integer of the number of days to grow macroalgae before harvesting.
#' @param temperature A vector of temperature readings from 
#' @param d_top 
#' @param hc 
#' @param farmV 
#' @param salmon 
#' @param alg 
#' @param nf 
#' @param QQ 
#' @param spec_params 
#' 
#' @importFrom lubridate ymd time_length yday is.Date duration
#' @inheritParams temp_atsite
#'
#' @return
#' @export
#'
#' @examples
grow_macroalgae <- function(start, grow_days, # DONE
                            temperature, 
                            light, 
                            nitrate, ammonium, 
                            other_N,
                            d_top, hc, farmV, salmon, alg, nf, QQ, site_params, spec_params,
                            ...) {

  # Get start and end dates
  if (is.Date(start)) {
    start_date <- start
  } else if (is.integer(start) | is.numeric(start)) {
    if (missing(start_year)){start_year = 2000}
    start_date <- (parse_date_time(x = paste(start_year, start), orders = "yj")) # - duration(1, "days")
  }
  end_date <- start_date + duration(grow_days, "days")
  start_t <- yday(start_date)
  end_t <- start_t + grow_days
  
  outputs <- data.frame(t = seq(start_t, end_t, 1), date = seq(start_date, end_date, by = 'days'))
  
  # Light, temperature and nutrient levels must be passed to the function as vectors - check length
  if (length(temperature) != nrow(outputs)) {
    stop("Error: temperature vector is length ", length(temperature), " but timspan vector is length ", nrow(outputs))
  }
  if (length(light) != nrow(outputs)) {
    stop("Error: light vector is length ", length(light), " but timspan vector is length ", nrow(outputs))
  }
  if (length(nitrate) != nrow(outputs)) {
    stop("Error: nitrate vector is length ", length(nitrate), " but timspan vector is length ", nrow(outputs))
  }
  if (length(ammonium) != nrow(outputs)) {
    stop("Error: ammonium vector is length ", length(ammonium), " but timspan vector is length ", nrow(outputs))
  }
  
  outputs$temperature <- temperature
  outputs$light <- light
  outputs$nitrate <- nitrate
  outputs$ammonium <- ammonium
  
  # Set up output data frame
  outputs <- cbind(dates, outputs, data.frame(matrix(ncol = 29, nrow = nrow(outputs))))
  colnames(outputs) <- c("t", "dates", "I", "Itop", "Tc", "Am1", "Ni1", "U", # first 8 colums are inputs
                           "Am", "Ni", "det", # next 3 columns are environmental states
                           "nf", "ns", "nperc", "B.dw.mg", "B.ww.mg", "hm", # next 6 columns are algae states
                           "uc", "lambda", "kma", "D", "Ct", # next 5 columns are environmental modifications from algae
                           "Qlim", "Tlim", "Ilim", # next 3 columns are algae limitations
                           "up_Am", "up_Ni", "growth_rate", "ns_to_nf", "nf_loss", "n_change") # next 6 columns are rates of N pool change
    
    # Set up input forcing
    hz <- as.numeric(site_params$wc_z[site_params$site == site])
    extra <- as.numeric(site_params$extra[site_params$site == site])
    
    # # Old forcing
    # outputs <- outputs %>% 
    #   mutate(I = I_a + I_c * sin((2 * pi * (t + I_b) + pi / 2) / 365),
    #          Tc = T_a + T_c * sin((2 * pi * (t + T_b) + pi / 2) / 365),
    #          Am1 = case_when(salmon == "Y" ~ 4.89154 + 12.8 + 4.95 * sin((2 * pi * (t+60) + pi / 2) / 365),
    #                          salmon == "N" ~ 4.89154),
    #          Ni1 = 24.2625 - 20.68 * sin((2 * pi * (t - 315) + pi / 2) / 365),
    #          U = 0.3 - 0.15 * sin((2 * pi * (t + 416) + pi / 2) / 365))
    
    # Starting conditions
    outputs$Am[1] <- outputs$Am1[1]
    outputs$Ni[1] <- outputs$Ni1[1]
    outputs$det[1] <- 10
    
    # Miscellaneous constants
    aa <- 0.2 ^ 2
    s <- 0.0045
    Cb <- 0.0025
    gam <- 1.13
    rL <- 0.2
    Rd <- 0.1
    kW <- 0.58 #0.65
    Hc <- (d_top + hc) / hz # farm/canopy proportion to wc (constant)
    
    # Set up all species parameters
    vam <- as.numeric(spec_params$vam[spec_params$species == alg])
    kam <- as.numeric(spec_params$kam[spec_params$species == alg])
    vni <- as.numeric(spec_params$vni[spec_params$species == alg])
    kni <- as.numeric(spec_params$kni[spec_params$species == alg])
    qmin <- as.numeric(spec_params$qmin[spec_params$species == alg])
    qmax <- as.numeric(spec_params$qmax[spec_params$species == alg])
    kc <- as.numeric(spec_params$kc[spec_params$species == alg])
    mu  <- as.numeric(spec_params$mu[spec_params$species == alg])
    dm  <- as.numeric(spec_params$dm[spec_params$species == alg])
    acs <- as.numeric(spec_params$acs[spec_params$species == alg])
    io <- as.numeric(spec_params$io[spec_params$species == alg])
    to <- as.numeric(spec_params$to[spec_params$species == alg])
    ti <- as.numeric(spec_params$ti[spec_params$species == alg])
    ta <- as.numeric(spec_params$ta[spec_params$species == alg])
    h_a <- as.numeric(spec_params$h_a[spec_params$species == alg])
    h_b <- as.numeric(spec_params$h_b[spec_params$species == alg])
    # h_c <- as.numeric(spec_params$h_c[spec_params$species == alg])
    dwww <- as.numeric(spec_params$dwww[spec_params$species == alg])
    nmin <- as.numeric(spec_params$nmin[spec_params$species == alg])
    nmax <- as.numeric(spec_params$nmax[spec_params$species == alg])
    
    # Macroalgae starting state
    outputs$nf[1] <- nf
    
      # Starting ns is based on specified Q
      Q <- qmax - QQ*(qmax - qmin)
      outputs$Qlim[1] <- Qlim <- (Q - qmin)/(Q - kc)
    
    outputs$ns[1] <- ns <- nf*(Q/qmin - 1)
    outputs$nperc[1] <- nperc <- nmax - QQ*(nmax - nmin)
    outputs$hm[1] <- hm <- (nf/(h_a*10^3))^h_b
    outputs$B.dw.mg[1] <- B.dw.mg <- (nf+ns) / nperc
    outputs$B.ww.mg[1] <- B.dw.mg * dwww
    
    # Start of actual model run
    for (i in 1:grow_days) {
      
      # Environmental state at start of day
      t <- outputs$t[i]
      I <- outputs$I[i]
      Tc <- outputs$Tc[i]
      U <- outputs$U[i]
      Am <- outputs$Am[i]
      Ni <- outputs$Ni[i]
      det <- outputs$det[i]
      
      # Environmental additions
      Ni1 <- outputs$Ni1[i]
      Am1 <- outputs$Am1[i]
      
      # Algae state at start of day
      nf <- outputs$nf[i]
      ns <- outputs$ns[i]
      nperc <- outputs$nperc[i]
      hm <- outputs$hm[i]
      # Based on previous day's state
        Hm <- min(hm/hz, 1)
        Hf <- min(hm/hc, 1)
      B.dw.mg <- outputs$B.dw.mg[i]
      B.ww.mg <- outputs$B.ww.mg[i]
      
      # Stored nutrients cannot exceed physiological limits
      Q <- qmin*(1 + (ns/nf))
      if(Q < qmin){
        Q <- qmin}
      if(Q > qmax){
        Q <- qmax}
      QQ <- (qmax - Q) / (qmax - qmin)
      outputs$Qlim[i] <- Qlim <- (Q - qmin)/(Q - kc)
      
      # Temperature limitation
      Tlim <- ((Tc - ta) * (Tc - ti) ^ 2) / ((to - ti) * ((to - ti) * (Tc - to) - (to - ta) * (to + ti - 2 * Tc)))
      outputs$Tlim[i] <- max(Tlim, 0)
      
      # Light limitation
      outputs$kma[i] <- kma <-  nf * acs * max(hm/d_top, 1) * 1/(min(hm, d_top))
      outputs$Itop[i] <- Itop <- I * exp(-kW*d_top)
      x <- exp(Itop/io)
      K <- kW+kma
      outputs$Ilim[i] <- Ilim <- (exp(1)/(K*d_top)) * ((1/x)^(1/exp(K*d_top)) - 1/x)
        #exp(-(I/io * exp(-dw) * exp(-cw))) - exp(-(I/io * exp(-dw)))
      
      # Attenuation
      outputs$D[i] <- D <- 0.5 * (0.0306/2) * Hf * (B.ww.mg/1000)
      Kd1 <- hz / 2 * D * s * U^(gam - 2)
      drag_test <- sqrt(Kd1 * (1 - Hc) * Hc * (Cb * Hc + aa) - aa * Cb * Hc)
      if (is.na(drag_test)) {uc <- ub <- 1} else {
        uc <- (-aa - Cb * Hc ^ 2 + (1 - Hc) * drag_test) / (Kd1 * Hc * (1 - Hc) ^ 3 - aa - Cb * Hc ^ 3)
        ub <- (1 - uc * Hc) / (1 - Hc)}
      outputs$uc[i] <- uc
      outputs$lambda[i] <- lambda <- (U*60*60*24)/farmV # U needs to be in m d-1
      outputs$Ct[i] <- Ct <- (Kd1 * Hc * uc ^ 2 + Cb * ub ^ 2) # Total drag coefficient
      
      # Nitrogen pool changes
      outputs$growth_rate[i] <- growth_rate <- max(mu * Ilim * Tlim * Qlim, 0)
      outputs$ns_to_nf[i] <- ns_to_nf <- max(growth_rate * ns, 0)
      #outputs$ns_loss[i] <- 
        ns_loss <- max(dm * ns, 0)
      outputs$nf_loss[i] <- nf_loss <- max(dm * nf, 0)
      red_Am <- Rd * Am
      remin <- rL * det
        # Cannot take up more N than is available
        outputs$up_Am[i] <- up_Am <- min(Am, QQ*(B.dw.mg/1000)*(vam*Am/(kam+Am)))
        outputs$up_Ni[i] <- up_Ni <- min(Ni, QQ*(B.dw.mg/1000)*(vni*Ni/(kni+Ni)))
      
      # New environmental states for tomorrow (cannot go below 0)
      outputs$Am[i+1] <- Am - up_Am + lambda*(Am1 - uc*Am) + ns_loss + remin - red_Am
      outputs$Ni[i+1] <- Ni - up_Ni + lambda*(Ni1 - uc*Ni) + red_Am
      outputs$det[i+1] <- det + nf_loss - remin
      
      # Algae starting state for tomorrow
      outputs$ns[i+1] <- ns + up_Am + up_Ni - ns_to_nf - ns_loss
      outputs$nf[i+1] <- nf + ns_to_nf - nf_loss
      outputs$n_change[i+1] <- up_Am + up_Ni - ns_loss - nf_loss
      outputs$hm[i+1] <- (outputs$nf[i]/(h_a*10^3))^h_b
      outputs$nperc[i+1] <- nmax - QQ*(nmax - nmin)
      outputs$B.dw.mg[i+1] <- (outputs$nf[i]+outputs$ns[i]) / (nmax - QQ*(nmax - nmin))
      outputs$B.ww.mg[i+1] <- dwww * (outputs$nf[i]+outputs$ns[i]) / (nmax - QQ*(nmax - nmin))
    }
    return(outputs)
  }
