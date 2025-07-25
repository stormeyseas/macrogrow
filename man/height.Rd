% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algae_height.R
\name{height}
\alias{height}
\title{Macroalgae height}
\usage{
height(Nf, spec_params)
}
\arguments{
\item{Nf}{Fixed nitrogen (mg m\eqn{^{-3}})}

\item{spec_params}{A vector of named numbers. Must include the parameters:
\itemize{
\item \code{h_max}, maximum species height. Can be \code{NA}.
\item \code{h_a}, \code{h_b} and \code{h_c}, parameters governing height change with \code{N_f}. If not supplied algae height will always be \code{h_max}.
}}
}
\value{
a scalar of macroalgae height (m)
}
\description{
Macroalgae height
}
\details{
Calculates macroalgae height as \eqn{h_m = \left(\frac{N_f}{h_a}\right)^{h_b} + h_c} up to a maximum of \code{h_max}.
Defaults are \eqn{h_a=1000}, \eqn{h_b=1} and \eqn{h_c=0}.
Algae height therefore defaults to \eqn{N_f \times 10^{-3}} if no parameters are supplied.
}
\examples{
my_species <- c(h_a = 750, h_b = 0.5, h_c = 0.01, h_max = 1)
Nf <- seq(100, 1000, 10)
height <- sapply(X = Nf, FUN = height, spec_params = my_species)
plot(Nf, height)
}
