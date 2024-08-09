#' @title DAEDALUS model for health, social and economic costs of a pandemic
#'
#' @description Run the DAEDALUS model from R. This is a work in progress.
#' @param country A country or territory name from among `country_names`.
#' @param epidemic A string for the infection parameter set to use.
#' Infection parameter sets may be known by the name of the outbreak or the
#' causative pathogen;
#' see **Details** for more on which epidemics are supported.
#' See `epidemic_names` for allowed names, as the name must match exactly.
#' Defaults to simulating an epidemic similar to Covid-19 outbreaks caused by
#' the wild type of the SARS-CoV-2 virus.
#'
#' Selecting an epidemic automatically pulls in infection parameters
#' associated with the epidemic; these are stored as packaged data in
#' `infection_data`.
#' Default infection parameters for epidemics can be over-ridden by passing them
#' as named arguments via `...`.
#' @param ... Additional arguments passed to `make_parameters()` or
#' `make_initial_state()`. See **Details** for permitted arguments.
#' @param time_end An integer-like value for the number of timesteps
#' at which to return data. This is treated as the number of days with data
#' returned for each day. Defaults to 300 days.
#'
#' @details
#'
#' ## Included epidemics
#'
#' Epidemics for which data are available are (pathogen in parentheses):
#' SARS 2004 (SARS-CoV-1), influenza 2009 (influenza A H1N1),
#' influenza 1957 (influenza A H2N2), influenza 1918 (influenza A H1N1),
#' Covid-19 wild type (SARS-Cov-2 wild type),
#' Covid-19 Omicron (SARS-CoV-2 omicron), Covid-19 Delta (SARS-CoV-2 delta).
#'
#' DAEDALUS allows users to substitute default model parameters with custom
#' values by passing them as `...` (see examples). These arguments are passed on
#' to the internal functions `make_parameters()` and `make_initial_state()` as
#' appropriate. Any arguments that are passed and are not suitable for those
#' functions will throw a warning.
#'
#' ## Initial state
#'
#' Users can pass the following initial state parameters, which are handled
#' internally by `make_initial_state()`:
#'
#' - `p_infectious`: A single numeric value in the range \eqn{[0.0, 1.0]} giving
#' the proportion of individuals in each age group and economic sector that are
#' to be initialised as infectious. Defaults to `1e-6`, or one in every one
#' million as infectious.
#'
#' - `p_asymptomatic`: A single numeric value in the range \eqn{[0.0, 1.0]} for
#' the proportion of initially infectious individuals who are considered to be
#' asymptomatic. Defaults to 0.0.
#'
#' ## Model parameters
#'
#' Users can pass the following infection and country parameters, which are
#' handled internally by `make_parameters()`:
#'
#' - `r0`: A single numeric value for the basic reproduction value of the
#' infection \eqn{R_0}.
#'
#' - `sigma`: A single numeric value > 0.0 for the rate of transition from the
#' exposed compartment to one of two infectious compartments.
#'
#' - `p_sigma`: A single numeric value in the range \eqn{[0.0, 1.0]} for the
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
#' - `gamma_H`: A numeric vector of length `N_AGE_GROUPS` (4) for the
#' age-specific recovery rate for individuals who are hospitalised.
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
#' - `demography`: A numeric vector of length `N_AGE_GROUPS` (4), giving the
#' number of individuals in each age group of interest. By default, this is
#' taken from `daedalus::country_data` based on the `country` selected.
#'
#' - `contact_matrix`: A square numeric matrix with `N_AGE_GROUPS` (4) rows and
#' columns, giving the per-capita number of contacts between individuals of each
#' age group. By default, this is taken from `daedalus::country_data` based on
#' the user-specified `country`.
#'
#' - `contacts_workplace`: A numeric vector of length `N_ECON_SECTORS` (45),
#' giving the per-capita number of worker-to-worker interactions within economic
#' sectors. Defaults to a standard value provided by the package as
#' `daedalus::economic_contacts`.
#'
#' - `contacts_consumer_worker`: A numeric matrix with `N_ECON_SECTORS` rows
#' (45) and `N_AGE_GROUPS` columns (4) giving the per-capita contacts between
#' individuals in each economic sector and consumers. Defaults to the
#' sector-wise workplace contacts distributed in proportion to country
#' demography.
#'
#' - `contacts_between_sectors`: A square numeric matrix with `N_ECON_SECTORS`
#' (45) rows and columns and with a zero diagonal, giving the number of
#' worker-to-worker contacts across economic sectors. Defaults to the package
#' standard of a null matrix as between-sector-contacts are not currently
#' modelled.
#'
#' @return A `<deSolve>` object.
#'
#' @examples
#' # with default infection parameters associated with an epidemic
#' output <- daedalus(country = "United Kingdom", epidemic = "influenza_1918")
#'
#' # with some infection parameters over-ridden by the user
#' output <- daedalus(
#'   country = "United Kingdom", epidemic = "influenza_1918",
#'   r0 = 3.0
#' )
#' @export
daedalus <- function(country,
                     epidemic = "sars_cov_2_pre_alpha",
                     ...,
                     time_end = 300) {
  # input checking
  # NOTE: names are case sensitive
  is_good_country <- checkmate::test_string(country) &&
    checkmate::test_subset(
      country, daedalus::country_names,
      empty.ok = FALSE
    )
  if (!is_good_country) {
    cli::cli_abort(
      c(
        "Expected `country` to be a single string giving a country name from
        among `country_names`.",
        i = "Country names are case sensitive and must be an exact match."
      )
    )
  }

  is_good_epidemic <- checkmate::test_string(epidemic) &&
    checkmate::test_subset(
      epidemic, names(daedalus::infection_data),
      empty.ok = FALSE
    )
  if (!is_good_epidemic) {
    cli::cli_abort(
      c(
        "Expected `epidemic` to be a string from among `epidemic_names`, but
        it is not.",
        i = "Epidemic names are case sensitive and must be an exact match."
      )
    )
  }

  is_good_time_end <- checkmate::test_count(time_end, positive = TRUE)
  if (!is_good_time_end) {
    cli::cli_abort(
      c(
        "Expected `time_end` to be a single positive integer-like number.",
        i = "E.g. `time_end = 100`, but not `time_end = 100.5`"
      )
    )
  }

  # capture parameters, check for numeric input, and split for lower-level fns
  parameters <- list(...)

  is_numeric_list <- checkmate::test_list(
    parameters,
    types = "numeric", any.missing = TRUE, all.missing = TRUE
  )
  if (!is_numeric_list) {
    cli::cli_abort(
      c(
        "Model options passed as `...` may only be numeric elements (numbers,
        and numeric vectors or matrices).",
        i = "See {.help daedalus::daedalus} Details for allowed parameter names
        and default values."
      )
    )
  }

  init_state_params <- parameters[names(parameters) %in%
    c("p_infectious", "p_asymptomatic")]

  initial_state <- do.call(
    make_initial_state,
    c(list(country = country), init_state_params)
  )

  parameters <- do.call(
    make_parameters,
    c(list(country = country, epidemic = epidemic), parameters[
      !names(parameters) %in% names(init_state_params)
    ])
  )

  data <- deSolve::lsoda(
    y = initial_state, times = seq.int(time_end),
    func = daedalus_rhs, parms = parameters
  )

  data
}
