#' Kd parameter from Secchi disk depth
#'
#' @description
#' Calculates the light attenuation coefficient in water via:
#' \deqn{\begin{array}[ccc] 
#' K_d(PAR) &=& 1.584 × SDD^{-0.894}
#' \end{array}}
#' where SDD is the Secchi disk depth. Equation is adapted from Zhang, Y., Liu, X., Yin, Y., Wang, M., & Qin, B. (2012). Predicting the light attenuation coefficient through Secchi disk depth and beam attenuation coefficient in a large, shallow, freshwater lake. Hydrobiologia, 693(1), 29–37. https://doi.org/10.1007/s10750-012-1084-2.
#' 
#' @param SDD Secchi disk depth (m)
#'
#' @return attenuation parameter for PAR
#' @export
#'
Kd_from_Secchi <- function(SDD) {
  1.584 * SDD^(-0.894)
}
