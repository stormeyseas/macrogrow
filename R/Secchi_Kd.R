#' @title Get light attenuation parameter Kd from Secchi depth
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
Secchi_to_Kd <- function(SDD) {
  1.584 * SDD^(-0.894)
}

#' @title Get Secchi depth from light attenuation parameter Kd
#'
#' @description
#' Calculates the light attenuation coefficient in water via:
#' \deqn{\begin{array}[ccc] 
#' K_d(PAR) &=& 1.584 × SDD^{-0.894}
#' \end{array}}
#' where SDD is the Secchi disk depth. Equation is adapted from Zhang, Y., Liu, X., Yin, Y., Wang, M., & Qin, B. (2012). Predicting the light attenuation coefficient through Secchi disk depth and beam attenuation coefficient in a large, shallow, freshwater lake. Hydrobiologia, 693(1), 29–37. https://doi.org/10.1007/s10750-012-1084-2.
#' 
#' @param Kd attenuation parameter for PAR
#'
#' @return Secchi disk depth (m)
#' @export
#'
Kd_to_Secchi <- function(Kd) {
  (Kd/1.584)^(-1/0.894)
}
