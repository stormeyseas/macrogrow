#' Uptake rate (Michaelis-Menton)
#'
#' @param conc external substrate concentration
#' @param V the maximum uptake rate \eqn{V_{max}}
#' @param K the half-saturation constant \eqn{K_{c}}
#'
#' @return the rate of uptake at the specified external concentration
#' @export
#'
#' @examples examples
MM_uptake <- function(conc, V, K) {
  uprate <- (V * conc / (K + conc))
  return(uprate)
}
