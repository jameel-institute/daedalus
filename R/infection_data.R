#' @title Infection characteristics for model epidemics
#' @name epidemic_data
#' @rdname epidemic_data
#'
#' @description Infection parameters used in DAEDALUS to simulate epidemics of
#' interest.
#'
#' `epidemic_names` provides the tags by which epidemics are known for users'
#' convenience.
#'
#' @details
#' Epidemics for which data are available are (pathogen in parentheses):
#'
#' - SARS 2004 (SARS-CoV-1)
#'
#' - Influenza 2009 (influenza A H1N1)
#'
#' - Influenza 1957 (influenza A H2N2)
#'
#' - Influenza 1918 (influenza A H1N1)
#'
#' - Covid-19 wild type (SARS-Cov-2 wild type)
#'
#' - Covid-19 Omicron (SARS-CoV-2 omicron)
#'
#' - Covid-19 Delta (SARS-CoV-2 delta).
#'
#' @format ## `infection_data`
#' A list with 7 elements, each corresponding to an epidemic (see **Details**),
#' and providing 10 infection parameters:
#'
#' - `r0`: A single numeric value for the basic reproduction value of the
#' infection \eqn{R_0}.
#'
#' - `sigma`: A single numeric value > 0.0 for the rate of transition from the
#' exposed compartment to one of two infectious compartments.
#'
#' - `p_sigma`: A single numeric value in the range \eqn{(0.0, 1.0)} for the
#' proportion of infectious individuals who are also symptomatic. Asymptomatic
#' individuals can have a different contribution to the force of infection from
#' symptomatic individuals.
#'
#' - `epsilon`: A single numeric value for the relative contribution of
#' asymptomatic infectious individuals to the force of infection (compared to
#' symptomatic individuals).
#'
#' - `gamma_Is`: A single numeric value for the recovery rate of infectious
#' individuals who are not hospitalised.
#'
#' - `gamma_Ia`: A single numeric value for the recovery rate from asymptomatic
#' infection.
#'
#' - `gamma_H`: A numeric vector of length 4 for the age-specific recovery rate
#' for individuals who are hospitalised.
#'
#' - `eta`: A numeric vector of length `N_AGE_GROUPS` (4) for the age-specific
#' hospitalisation rate for individuals who are infectious and symptomatic.
#'
#' - `omega`: A numeric vector of length `N_AGE_GROUPS` (4) for the age-specific
#' mortality rate for individuals who are hospitalised.
#'
#' - `rho`: A single numeric value for the rate at which infection-derived
#' immunity wanes, returning individuals in the 'recovered' compartment to the
#' 'susceptible' compartment.
#'
#' @source See processing details in `data-raw/infection_data.R
#' @examples
#' # check available epidemics
#' epidemic_names
"infection_data"

#' @name epidemic_data
#' @format ## `infection_parameter_names`
#' A character vector with 10 names for the infection parameters in
#' `infection_data`. Mainly for internal use.
"infection_parameter_names"

#' @name epidemic_data
#' @format ## `epidemic_names`
#' A character vector with 7 elements.
"epidemic_names"
