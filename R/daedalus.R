#' @title DAEDALUS model for health, social and economic costs of a pandemic
#'
#' @description Run the DAEDALUS model from R. This is a work in progress.
#' @param country A country or territory name from among `country_names`.
#' @param ... Additional arguments passed to `make_parameters()` or
#' `make_initial_state()`. See **Details** for permitted arguments.
#' @param time_end An integer-like value for the number of timesteps
#' at which to return data. This is treated as the number of days with data
#' returned for each day. Defaults to 300 days.
#'
#' @details
#'
#' DAEDALUS allows users to substitute default model parameters with custom
#' values by passing them as `...` (see examples). These arguments are passed on
#' to the internal functions `make_parameters()` and `make_initial_state()` as
#' appropriate. Any arguments that are passed and are not suitable for those
#' functions will throw a warning.
#'
#' ## Initial state
#'
#' `make_initial_state()` accepts the following parameters:
#'
#' - `p_infectious`: A single numeric value in the range \eqn{(0.0, 1.0)} giving
#' the proportion of individuals in each age group and economic sector that are
#' to be initialised as infectious. Defaults to `1e-6`, or one in every one
#' million as infectious.
#'
#' - `p_asymptomatic`: A single numeric value in the range \eqn{(0.0, 1.0)} for
#' the proportion of initially infectious individuals who are considered to be
#' asymptomatic. Defaults to 0.0.
#'
#' ## Model parameters
#'
#' `make_parameters()` allows users to pass the following pathogen parameters:
#'
#' - `beta`: A single numeric value > 0.0 for the infection transmission rate
#' \eqn{\beta}. Defaults to 0.0186, assuming an \eqn{R_0} of 1.3
#' and an infectious period (\eqn{T_I}) of 7.0 days
#' (for \eqn{\beta = R_0 / T_I}).
#'
#' - `sigma`: A single numeric value > 0.0 for the rate of transition from the
#' exposed compartment to one of two infectious compartments. Defaults to 0.333
#' assuming a pre-infectious period of 3.0 days.
#'
#' - `p_sigma`: A single numeric value in the range \eqn{(0.0, 1.0)} for the
#' proportion of infectious individuals who are also symptomatic. Asymptomatic
#' individuals can have a different contribution to the force of infection from
#' symptomatic individuals. Defaults to 0.66.
#'
#' - `epsilon`: A single numeric value for the relative contribution of
#' asymptomatic infectious individuals to the force of infection (compared to
#' symptomatic individuals). Defaults to 0.2.
#'
#' - `gamma`: A single numeric value for the recovery rate. Defaults to 0.143,
#' assuming an infectious period of 7.0 days.
#'
#' - `eta`: A single numeric value for the hospitalisation rate of symptomatic
#' infectious individuals. Defaults to 0.01, assuming one of every hundred
#' symptomatic individuals needs hospitalisation.
#'
#' - `omega`: A single numeric value for the mortality rate of hospitalised
#' individuals. Defaults to 0.01, assuming one in every hundred hospitalisations
#' results in death.
#'
#' - `rho`: A single numeric value for the rate at which infection-derived
#' immunity wanes, returning individuals in the 'recovered' compartment to the
#' 'susceptible' compartment. Defaults to 0.00556, assuming that such immunity
#' lasts on average 180 days.
#'
#' - `demography`: A numeric vector of length `N_AGE_GROUPS` (4), giving the
#' number of individuals in each age group of interest. By default, this is
#' taken from `country_data` based on the `country` selected.
#'
#' - `contact_matrix`: A square numeric matrix with `N_AGE_GROUPS` rows and
#' columns, giving the per-capita number of contacts between individuals of each
#' age group. By default, this is taken from `country_data` based on `country`.
#'
#' - `contacts_workplace`: A numeric vector of length `N_ECON_SECTORS` (45),
#' giving the per-capita number of worker-to-worker interactions within economic
#' sectors. Defaults to a standard value provided by the package as
#' `economic_contacts`.
#'
#' - `contacts_consumer_worker`: A numeric matrix with `N_ECON_SECTORS` rows
#' (45) and `N_AGE_GROUPS` columns (4) giving the per-capita contacts between
#' individuals in each economic sector and consumers. Defaults to the
#' sector-wise workplace contacts distributed in proportion to country
#' demography.
#'
#' - `contacts_between_sectors`: A square numeric matrix with `N_ECON_SECTORS`
#' rows and columns and with a zero diagonal, giving the number of
#' worker-to-worker contacts across economic sectors. Defaults to the package
#' standard of a null matrix as between-sector-contacts are not currently
#' modelled.
#'
#' @return A `<deSolve>` object.
#' @export
daedalus <- function(country, ...,
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
        among `country_names`, but it is not.",
        i = "Country names are case sensitive and must be an exact match."
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
    c(list(country = country), parameters[
      !names(parameters) %in% names(init_state_params)
    ])
  )

  data <- deSolve::lsoda(
    y = initial_state, times = seq.int(time_end),
    func = daedalus_rhs, parms = parameters
  )

  data
}
