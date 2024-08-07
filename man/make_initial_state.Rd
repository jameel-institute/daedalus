% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{make_initial_state}
\alias{make_initial_state}
\title{Generate a default initial state for DAEDALUS}
\usage{
make_initial_state(country, p_infectious = 1e-06, p_asymptomatic = 0)
}
\arguments{
\item{country}{A string for the country name.}

\item{p_infectious}{A single value between 0.0 and 1.0 for the proportion of
individuals infectious This is applied to individuals in all economic
sectors.}

\item{p_asymptomatic}{A single value between 0.0 and 1.0 for the proportion
of infections that are asymptomatic.}
}
\value{
AN array with as many dimensions as \code{N_ECON_STRATA} (currently, 46)
with each layer giving the proportion of individuals of each group in each
epidemiological compartment.
}
\description{
Function to prepare the model initial state.
}
\keyword{internal}
