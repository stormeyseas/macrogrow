#' Relative nutrient quotient
#'
#' @param Q_int Non-dimensionalised internal nutrient quotient
#' @param spec_params A vector of named numbers. Must include:
#'  * `Q_min`, the minimum internal nutrient quotient
#'  * `Q_max`, the maximum internal nutrient quotient
#'
#' @return the relative (0-1) internal nutrient quotient
#' @export
#'
Q_rel <- function(Nf = NULL, Ns = NULL, Q_int = NULL, spec_params) {
  if (!is.null(Nf) & !is.null(Ns)) {
    Q_int <- Q_int(Nf = Nf, Ns = Ns, spec_params = spec_params)
  }
  if (!is.null(Q_int)) {
    Q_rel <- 1 - (spec_params['Q_max'] - Q_int)/(spec_params['Q_max'] - spec_params['Q_min'])
  }
  return(unname(Q_rel))
}

