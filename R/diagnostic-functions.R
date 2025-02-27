#' Plot of species growth response over a range of temperatures
#' 
#' @description
#' A shortcut function to quickly see a species' response to a range of temperatures.
#' 
#' @inheritParams T_lim
#' @param temp a vector of the range of temperatures to be tested (defaults to seq(-10, 45, 0.25))
#'
#' @import ggplot2
#' @return a ggplot object of growth response over the specified temperature range
#'
#' @examples 
#' spec_params <- c(T_opt = 25, T_min = 5, T_max = 35)
#' T_plot(spec_params)
#' 
#' @seealso [T_lim()]
T_plot = function(spec_params, temp = seq(-10,45,0.25)){
  df <- data.frame(
    temperature = temp, 
    T_lim = sapply(X = temp, FUN = T_lim, spec_params = spec_params)
  )
  ggplot2::ggplot(df, aes(x = temperature, y = T_lim)) +
    geom_line() + theme_classic() + 
    scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0,1))
}

#' Plot of species growth response over a range of surface irradience values
#'
#' @inheritParams I_lim
#' @param irr a vector of the range of surface irradiance values to be tested (defaults to seq(0, 2500, 10))
#'
#' @import ggplot2
#' @return a ggplot object of growth response over the specified range of surface irradience values
#'
#' @examples 
#' my_species <- c(I_o = 150, a_cs = 0.005, h_max = 1)
#' my_site <- c(kW = 0.6, d_top = 1)
#' I_plot(Nf = 150, spec_params = my_species, site_params = my_site)
#' 
#' @seealso [I_lim() algae_height()]
I_plot = function(Nf, spec_params, site_params, irr = seq(0, 2500, 10)){
  df <- data.frame(
    surface_irradiance = irr, 
    I_lim = sapply(X = irr, FUN = I_lim, Nf = Nf, spec_params = spec_params, site_params = site_params)
  )
  ggplot2::ggplot(df, aes(x = surface_irradiance, y = I_lim)) + 
    geom_line() + theme_classic() + 
    scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0,1))
}

#' Plot height change over a range of fixed-N values
#'
#' @inheritParams algae_height
#'
#' @return a ggplot object of growth response over the specified range of fixed-N values
#'
#' @examples examples
hm_plot <- function(spec_params){}

#' Plot a species uptake rate over a range of N concentrations
#'
#' @inheritParams get_uptake
#' @param conc a vector of the range of N concentrations to be tested (defaults to seq(0, 50, 0.25))
#'
#' @return a ggplot object of nitrogen uptake rates
#'
#' @examples examples
N_plot <- function(spec_params, uptake_shape, abbreviations = c("ni" = "nitrate", "am" = "ammonium", "ot" = "other")){}

  