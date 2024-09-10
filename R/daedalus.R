#' @title DAEDALUS model for health, social and economic costs of a pandemic
#'
#' @description Run the DAEDALUS model from R. This is a work in progress.
#'
#' @param country A country or territory name from among `country_names`.
#' Country-specific data such as the community and workplace contacts, the
#' demography, and the distribution of the workforce into economic sectors is
#' automatically accessed from package data for the relevant country name.
#' Some contact data values can be over-ridden by the user by passing them as a
#' named list to `country_params_manual`.
#'
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
#' as a named list to `infection_params_manual`.
#'
#' @param response_strategy A string for the name of response strategy followed;
#' defaults to "none". The response strategy determines the country-specific
#' response threshold following which the response is activated. See
#' `response_threshold`.
#'
#' While the response strategy is active, economic contacts are scaled using the
#' package data object `daedalus::closure_data`.
#'
#' @param implementation_level A string for the level at which the strategy is
#' implemented; defaults to "light".
#'
#' @param response_threshold A single numeric value for the total number of
#' hospitalisations that causes an epidemic response
#' (specified by `response_strategy`) to be triggered, if it has not already
#' been triggered via `response_time`. Currently defaults to 1000, which
#' overrides the default response- and country-specific threshold values held in
#' [daedalus::country_data].
#'
#' @param response_time A single numeric value for the time in days
#' at which the selected response is activated. This is ignored if the response
#' has already been activated by the hospitalisation threshold being reached.
#' Defaults to 30 days.
#'
#' @param country_params_manual An optional **named** list of country-specific
#' contact data. See **Details** for allowed values.
#'
#' @param infect_params_manual An optional **named** list of infection
#' parameters that can be passed to over-ride the default values for the chosen
#' `epidemic`. See **Details** for allowed values.
#'
#' @param initial_state_manual An optional **named** list with the names
#' `p_infectious` and `p_asymptomatic` for the proportion of infectious and
#' symptomatic individuals in each age group and economic sector.
#' Defaults to `1e-6` and `0.0` respectively.
#'
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
#' values by passing them in the appropriate argument (see examples).
#' These arguments are passed on to the internal functions
#' `make_country_parameters()` and `make_infection_parameters()` as
#' appropriate. Any arguments that are passed and are not suitable for those
#' functions will throw an error.
#'
#' ## Initial state
#'
#' Users can pass the following initial state parameters to
#' `initial_state_manual`:
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
#' ## Country parameters
#'
#' `country_params_manual` may be a named list of one or more of the following:
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
#' ## Infection parameters
#'
#' `infect_params_manual` may be a named list of one or more of the following:
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
#' @return A `<deSolve>` object.
#'
#' @examples
#' # with default infection parameters associated with an epidemic
#' output <- daedalus(country = "United Kingdom", epidemic = "influenza_1918")
#'
#' # with some infection parameters over-ridden by the user
#' output <- daedalus(
#'   country = "United Kingdom", epidemic = "influenza_1918",
#'   infect_params_manual = list(r0 = 1.3)
#' )
#'
#' # with some country parameters over-ridden by the user
#' output <- daedalus(
#'   country = "United Kingdom", epidemic = "influenza_1918",
#'   country_params_manual = list(contact_matrix = matrix(1, 4, 4))
#' )
#'
#' # with default initial conditions over-ridden by the user
#' output <- daedalus(
#'   country = "United Kingdom", epidemic = "influenza_1918",
#'   initial_state_manual = list(p_infectious = 1e-3)
#' )
#' @export
daedalus <- function(country,
                     epidemic,
                     response_strategy = c(
                       "none", "elimination", "economic_closures",
                       "school_closures"
                     ),
                     implementation_level = c("light", "heavy"),
                     response_time = 30,
                     response_threshold = 1000,
                     country_params_manual = list(),
                     infect_params_manual = list(),
                     initial_state_manual = list(),
                     time_end = 300) {
  # input checking
  # NOTE: names are case sensitive
  country <- rlang::arg_match(country, daedalus::country_names)
  epidemic <- rlang::arg_match(epidemic, daedalus::epidemic_names)

  response_strategy <- rlang::arg_match(response_strategy)
  implementation_level <- rlang::arg_match(implementation_level)

  is_good_time_end <- checkmate::test_count(time_end, positive = TRUE)
  if (!is_good_time_end) {
    cli::cli_abort(
      c(
        "Expected `time_end` to be a single positive integer-like number.",
        i = "E.g. `time_end = 100`, but not `time_end = 100.5`"
      )
    )
  }

  is_good_response_time <- checkmate::test_integerish(
    response_time,
    upper = time_end - 2L, lower = 2L, any.missing = FALSE
  )
  if (!is_good_response_time) {
    cli::cli_abort(
      "Expected `response_time` to be between 2 and {time_end - 2L}."
    )
  }

  is_good_threshold <- checkmate::test_count(
    response_threshold,
    positive = TRUE
  )
  if (!is_good_threshold) {
    cli::cli_abort(
      "Expected `response_threshold` to be a positive finite integer."
    )
  }

  initial_state <- as.numeric(make_initial_state(country, initial_state_manual))

  parameters <- c(
    make_country_parameters(country, country_params_manual),
    make_infection_parameters(epidemic, infect_params_manual)
  )

  # NOTE: using {rlang} for convenience
  mutables <- rlang::env(switch = 0.0)

  # add the appropriate economic openness vectors to parameters
  openness <- daedalus::closure_data[[
    response_strategy
  ]][[implementation_level]]

  parameters <- c(
    parameters,
    list(
      openness = openness,
      mutables = mutables,
      min_time = 1 # setting minimum time to prevent switch flipping
    )
  )

  # get activation and termination events
  activation_event <- make_response_threshold_event(response_threshold)
  termination_event <- make_rt_end_event() # NOTE: only type of end event as yet

  # two-stage model run: run from 1:response_time with switch = 0.0, or off
  # from response_time:time_end run with switch = 1.0, or on
  # NOTE: state is carried over. This looks ugly and might not scale if
  # parameter uncertainty is needed in future.
  times_stage_one <- seq(1, response_time)
  times_stage_two <- seq(response_time, time_end)

  data_stage_one <- deSolve::lsoda(
    y = initial_state, times = times_stage_one,
    func = daedalus_rhs, parms = parameters,
    rootfunc = activation_event[["root_function"]],
    events = list(func = activation_event[["event_function"]], root = TRUE)
  )

  # carry over initial state; could be named more clearly?
  initial_state <- data_stage_one[
    nrow(data_stage_one), colnames(data_stage_one) != "time"
  ]
  initial_state <- as.numeric(initial_state)

  # set switch parameter
  rlang::env_poke(parameters[["mutables"]], "switch", 1.0)

  # reset min time
  parameters[["min_time"]] <- response_time

  data_stage_two <- deSolve::lsoda(
    initial_state, times_stage_two,
    daedalus_rhs, parameters,
    rootfunc = termination_event[["root_function"]],
    events = list(func = termination_event[["event_function"]], root = TRUE)
  )

  data <- rbind(data_stage_one, data_stage_two[-1, ])

  prepare_output(data)
}
