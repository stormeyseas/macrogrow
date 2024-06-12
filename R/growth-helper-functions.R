#' Calculate Q limitation
#'
#' @param Q 
#' @param Qmin 
#' @param Qmax 
#' @param Kc 
#'
#' @return
#' 
Q_lim <- function(Nf, Ns, Q_min, Q_max, K_c) {
  Q <- Q_min * (1 + Ns/Nf)
  if (Q < Q_min) {Q <- Q_min} 
  if (Q > Q_max) {Q <- Q_max}
  
  Qlim <- (Q - Q_min)/(Q - K_c)
  return(Qlim)
}

#' Calculate macroalgae height
#'
#' @param Nf 
#' @param a 
#' @param b 
#' @param c 
#'
#' @return
#' @export
#'
#' @examples
algae_height <- function(Nf, a, b, c) {
  hm <- (Nf/a)^b + c
}

#' Calculate light limitation
#'
#' @param Nf 
#' @param I_o 
#' @param a_cs 
#' @param hm 
#' @param d_top 
#' @param I 
#' @param kW 
#'
#' @return
#' @export
#'
#' @examples
I_lim <- function(Nf, I_o, a_cs, hm, d_top, I, kW = 0.65) {
  k_ma <-  Nf * a_cs * max(hm / d_top, 1) * 1 / (min(hm, d_top))
  Itop <- I * exp(-kW * d_top)
  
  x <- exp(Itop / I_o)
  K <- kW + k_ma
  
  Ilim <- (exp(1) / (K * d_top)) * ((1 / x) ^ (1 / exp(K * d_top)) - 1 / x)
  #Ilim <- exp(-(I/I_o * exp(-dw) * exp(-cw))) - exp(-(I/I_o * exp(-dw)))
  
  return(Ilim)
}

#' Temperature limitation (CTMI) on growth
#' 
#' Given species parameters returns the relative limitation on growth rate according to a CTMI curve.
#'
#' @param TT temperature to evaluate
#' @param T_max Maximum temperature for macroalgae growth
#' @param T_opt Optimum temperature for macroalgae growth
#' @param T_min Minimum temperature for macroalgae growth
#'
#' @return A dataframe with relative temperature limitation (between 0 and 1) at each timestep.
#' @export
#'
#' @examples
#' 
T_lim <- function(TT, T_max, T_opt, T_min){
  
  if (T_opt < T_min) {stop("Error: minimum temperature is higher than optimum temperature")}
  if (T_opt > T_max) {stop("Error: maximum temperature is lower than optimum temperature")} 
  if (T_opt-T_min <= T_max-T_opt) {stop("Species CTMI function not valid! Must satisfy T_opt-T_min > T_max-T_opt")}
  
  if (TT > T_max) {Tlim <- 0
  } else if (TT < T_min) {Tlim <- 0
  } else {Tlim <- ((TT - T_max)*(TT - T_min)^2)/((T_opt - T_min)*((T_opt - T_min)*(TT - T_opt) - (T_opt - T_max)*(T_opt + T_min - 2*TT)))}
  
  return(Tlim)
}

#' Title
#'
#' @param V_am 
#' @param K_am 
#' @param V_ni 
#' @param K_ni 
#' @param site_df 
#'
#' @return
#' @export
#'
#' @examples
#' 
Nlim_atsite <- function(V_am, K_am, V_ni, K_ni, site_df){
  
  colnames(site_df) = c("t", "Nitrate", "Ammonia")
  
  year_N = site.N %>% 
    mutate(Uptake_nitrate = (V_ni * Nitrate)/(K_ni + Nitrate),
           Uptake_ammonium = (V_am * Ammonia)/(K_am + Ammonia))
  
  return(year_N)
}

#' Calculate relative attenuation
#'
#' @param U0 incoming incident water velocity (m/s)
#' @param hm macroalgae height (m)
#' @param hz total water depth (m)
#' @param hc vertical water column occupied by the canopy (m)
#' @param d_top depth of the canopy beneath the water surface (m)
#' @param SA_WW conversion of wet weight to surface area (default is 0.5*0.5*0.0306 for \textit{Macrocystis pyrifera})
#' @param biomass wet weight of macroalgae (g)
#' @param s 
#' @param gam 
#' @param aa 
#' @param Cb 
#'
#' @return
#' @export
#'
#' @examples
attenuate <- function(U0, hm, hz, hc, d_top, SA_WW = 0.5 * (0.0306/2), biomass, s = 0.0045, gam = 1.13, aa = 0.2^2, Cb = 0.0025){
  D <- SA_WW * min(hm/hc, 1) * biomass
  Kd <- 0.5 * hz * D * s * U0^(gam - 2)
  Hc <- (d_top + hc) / hz

  drag_test <- sqrt(Kd * (1 - Hc) * Hc * (Cb * Hc + aa) - aa * Cb * Hc)
  if (is.na(drag_test)) {
    uc <- 1
    } else {
      uc <- (-aa - Cb * Hc ^ 2 + (1 - Hc) * drag_test) / (Kd * Hc * (1 - Hc) ^ 3 - aa - Cb * Hc ^ 3)
    }

  # ub <- (1 - uc * Hc) / (1 - Hc)
  # Ct <- (Kd * Hc * uc ^ 2 + Cb * ub ^ 2) # Total drag coefficient
  
  return(uc)
}

#' Nitrogen uptake rate
#'
#' @param conc 
#' @param Q 
#' @param biomass 
#' @param V 
#' @param K 
#'
#' @return
#' @export
#'
#' @examples
N_uptake <- function(conc, Q, biomass, V, K) {
  uprate <- min(conc, Q * biomass * (V * conc / (K + conc)))
  return(uprate)
}

