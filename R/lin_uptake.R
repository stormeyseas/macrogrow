#' Uptake rate (linear)
#' 
#' @inheritParams MM_uptake
#' @param M the slope of N uptake with increasing substrate concentration 
#' @param C the intercept
#'
#' @return the rate of uptake at the specified external concentration
#' @export
#'
#' @examples examples
lin_uptake <- function(conc, M, C) {
  uprate <- M * conc + C
  return(uprate)
}
