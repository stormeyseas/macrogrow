% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grow_macroalgae.R
\name{grow_macroalgae}
\alias{grow_macroalgae}
\title{Grow macroalgae}
\usage{
grow_macroalgae(
  start = 1,
  grow_days,
  temperature,
  salinity,
  light,
  kW,
  velocity,
  nitrate,
  ammonium,
  other_N = NA,
  ni_uptake,
  am_uptake,
  ot_uptake = NA,
  site_params,
  spec_params,
  initials,
  sparse_output = T,
  other_constants = c(s = 0.0045, gam = 1.13, a2 = 0.2^2, Cb = 0.0025)
)
}
\arguments{
\item{start}{integer, start of the growth period (day of at-sea deployment). Defaults to 1.}

\item{grow_days}{integer, number of day in growing period - if missing will take the length of the temperature vector}

\item{temperature}{a vector of daily temperatures (\eqn{^{\circ}}C)}

\item{salinity}{a vector of daily salt concentrations (g L\eqn{^{-1}})}

\item{light}{a vector of surface light (umol m\eqn{^{-2}} s\eqn{^{-1}})}

\item{kW}{a vector of light attenuation coefficients for open water (m\eqn{^{-1}})}

\item{velocity}{a vector of water velocities (m s\eqn{^{-1}})}

\item{nitrate}{a vector of nitrate concentrations (mg m\eqn{^{-3}})}

\item{ammonium}{a vector of ammonium concentrations (mg m\eqn{^{-3}})}

\item{other_N}{a vector of other nitrogen concentrations (mg N m\eqn{^{-3}}) - NOT IN USE}

\item{site_params}{a named vector of species-specific parameters - see details}

\item{spec_params}{a named vector of site-specific parameters - see details}

\item{initials}{a named vector of the macroalgae starting conditions}

\item{sparse_output}{logical, whether to include input vectors and other non-essential information in final dataframe (default = TRUE)}

\item{other_constants}{a named vector of miscellaneous constants (see \code{u_c()})}
}
\value{
matrix of outputs
}
\description{
Initiate and grow macroalgae.
}
\details{
Example csv with all the spec_params & site_params required?
\itemize{
\item Note that the final growth dataframe is inclusive of the start and end date, so the environmental vectors must be the same
}
}
\examples{
"see here" link?
}
\seealso{
\code{\link[=check_grow]{check_grow()}}
}
