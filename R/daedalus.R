#' Initial values for model flags
#'
#' @return A vector of initial flag values; all flags are initially set to 'off'
#' or 0.0.
#'
#' @keywords internal
initial_flags <- function() {
  vax_flag <- 0.0
  npi_flag <- 0.0
  ipr <- 0.0 # incidence-prevalence ratio

  c(ipr = ipr, npi_flag = npi_flag, vax_flag = vax_flag)
}

#' Get model response times from dust2 output
#'
#' @param event_data dust2 output event data from `daedalus_internal()`.
#' @param time_end The model end time, passed from [daedalus()].
#'
#' @return A vector of event start and end times suitable for a
#' `<daedalus_output>` object. Returns model end time if there is no response
#' end time.
#'
#' @keywords internal
get_daedalus_response_times <- function(event_data, time_end) {
  # internal function with no input checking
  resp_times_on <- event_data[grepl("npi_\\w*_on$", event_data$name), "time"]
  resp_time_on_realised <- if (length(resp_times_on) == 0) {
    NA_real_
  } else {
    min(resp_times_on)
  }

  resp_times_off <- event_data[grepl("npi_\\w*_off$", event_data$name), "time"]
  resp_time_off_realised <- if (is.na(resp_time_on_realised)) {
    NA_real_
  } else if (length(resp_times_off) == 0) {
    time_end
  } else {
    min(resp_times_off)
  }

  duration <- resp_time_off_realised - resp_time_on_realised

  # return list for consistency with daedalus
  list(
    closure_time_start = resp_time_on_realised,
    closure_time_end = resp_time_off_realised,
    closure_duration = duration
  )
}

#' Internal function for daedalus
#'
#' @return A list of state values as returned by `dust2::dust_unpack_state()`.
#' @keywords internal
daedalus_internal <- function(
  time_end,
  params,
  state,
  flags,
  ode_control,
  n_groups
) {
  sys <- dust2::dust_system_create(
    daedalus_ode,
    params,
    n_groups = n_groups,
    ode_control = ode_control
  )

  # add initial flags
  state <- c(state, flags)
  dust2::dust_system_set_state(sys, state)

  state <- dust2::dust_system_simulate(sys, seq(0, time_end))

  list(
    data = dust2::dust_unpack_state(sys, state),
    event_data = dust2::dust_system_internals(sys)[["events"]]
  )
}

#' @title DAEDALUS model for health, social and economic costs of a pandemic
#'
#' @description Run the DAEDALUS model from R. This is a work in progress.
#'
#' @param country A country or territory object of class `<daedalus_country>`,
#' **or** a country or territory name from those included in the package;
#' see [daedalus.data::country_names], **or** a country ISO2 or ISO3 code; see
#' [daedalus.data::country_codes_iso2c] and
#' [daedalus.data::country_codes_iso3c].
#' Country-specific data such as the community and workplace contacts, the
#' demography, and the distribution of the workforce into economic sectors is
#' automatically accessed from package data for the relevant country name if it
#' is passed as a string.
#' To override package defaults for country characteristics, pass a
#' `<daedalus_country>` object instead. See [daedalus_country()] for more.
#'
#' @param infection An infection parameter object of the class
#' `<daedalus_infection>`, **or** an epidemic name for which data are provided
#' in the package; see [daedalus.data::epidemic_names] for historical epidemics
#' or epidemic waves for which parameters are available.
#' Passing the name as a string automatically accesses the default parameters
#' of an infection. Create and pass a `<daedalus_infection>` to tweak infection
#' parameters using [daedalus_infection()].
#'
#' @param response_strategy A string for the name of response strategy followed;
#' defaults to "none". The response strategy determines the country-specific
#' response threshold following which the response is activated. See
#' `response_threshold`.
#'
#' While the response strategy is active, economic contacts are scaled using the
#' package data object `daedalus.data::closure_data`.
#'
#' @param vaccine_investment Either a single string or a
#' `<daedalus_vaccination>` object specifying the vaccination parameters
#' associated with an advance vaccine-investment scenario. Defaults to `NULL`
#' for absolutely no vaccination in the model.
#' A vaccination investment of `"none"` indicates no _prior_ investment, but
#' the model will include vaccination beginning after 1 year, at a low rate
#' across all age groups. Other accepted values are `"low"`, `"medium"` and
#' `"high"`. See [daedalus_vaccination()] for more information.
#'
#' @param response_time A single numeric value for the time in days
#' at which the selected response is activated. This is ignored if the response
#' has already been activated by the hospitalisation threshold being reached.
#' Defaults to 30 days.
#'
#' @param response_duration A single integer-ish number that gives the number of
#' days after `response_time` that an NPI should end.
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
#' @param ... Optional arguments that are passed to [dust2::dust_ode_control()].
#'
#' @details
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
#' @return A `<daedalus_output>` object if `infection` is a string or a single
#' `<daedalus_infection>` object. Otherwise, a list of `<daedalus_output>`s
#' of the same length of `infection` if a list of `<daedalus_infection>`s is
#' passed to `infection`.
#'
#' @examples
#' # country and infection specified by strings using default characteristics
#' output <- daedalus(
#'   "Canada", "influenza_1918"
#' )
#'
#' # country passed as <daedalus_country> with some characteristics modified
#' country_x <- daedalus_country(
#'   "Canada",
#'   parameters = list(contact_matrix = matrix(5, 4, 4)) # uniform contacts
#' )
#' output <- daedalus(country_x, "influenza_1918")
#'
#' # with some infection parameters over-ridden by the user
#' output <- daedalus(
#'   "United Kingdom",
#'   daedalus_infection("influenza_1918", r0 = 1.3)
#' )
#'
#' # with default initial conditions over-ridden by the user
#' output <- daedalus(
#'   "United Kingdom", "influenza_1918",
#'   initial_state_manual = list(p_infectious = 1e-3)
#' )
#'
#' @export
daedalus <- function(
  country,
  infection,
  response_strategy = NULL,
  vaccine_investment = NULL,
  response_time = 30,
  response_duration = 365,
  initial_state_manual = NULL,
  time_end = 600,
  ...
) {
  # prepare flags
  flags <- initial_flags()

  # input checking
  country <- validate_country_input(country)

  # handle infection input and convert to a list of daedalus_infection
  infection <- validate_infection_input(infection)

  # collect optional ODE control params and create ode_control obj
  ode_control <- rlang::list2(...)
  if (length(ode_control) > 0) {
    ode_control <- do.call(dust2::dust_ode_control, ode_control)
  } else {
    ode_control <- NULL
  }

  # checks on interventions
  # also prepare the appropriate economic openness vectors
  # allowing for a numeric vector, or NULL for truly no response
  if (is.null(response_strategy)) {
    response_strategy <- "none" # for output class
    openness <- rep(1.0, N_ECON_SECTORS)
    response_time <- NULL # to be filtered out later
  } else if (is.numeric(response_strategy)) {
    checkmate::assert_numeric(
      response_strategy,
      lower = 0,
      upper = 1,
      len = N_ECON_SECTORS
    )
    openness <- response_strategy
  } else if (
    length(response_strategy) == 1 &&
      response_strategy %in% names(daedalus.data::closure_data)
  ) {
    openness <- daedalus.data::closure_data[[response_strategy]]
  } else {
    cli::cli_abort(
      "Got an unexpected value for `response_strategy`. Options are `NULL`, \
      a numeric vector, or a recognised strategy. See function docs."
    )
  }

  # checks on vaccination
  vaccine_investment <- validate_vaccination_input(vaccine_investment)

  if (get_data(vaccine_investment, "start_time") == 0.0) {
    # check vaccination start time and set vaccination flag
    flags["vax_flag"] <- 1.0
  }

  is_good_time_end <- checkmate::test_count(time_end, positive = TRUE)
  if (!is_good_time_end) {
    cli::cli_abort(c(
      "Expected `time_end` to be a single positive integer-like number.",
      i = "E.g. `time_end = 100`, but not `time_end = 100.5`"
    ))
  }

  # NULL converted to "none"; WIP: this will be moved to a class constructor
  if (!identical(response_strategy, "none")) {
    is_good_response_time <- checkmate::test_integerish(
      response_time,
      upper = time_end,
      lower = 1L, # responses cannot start at 0, unless strategy is null
      any.missing = FALSE,
      len = 1
    )
    if (!is_good_response_time) {
      cli::cli_abort(
        "Expected `response_time` to be between 1 and {time_end}."
      )
    }

    is_good_response_duration <- checkmate::test_count(
      response_duration
    )
    if (!is_good_response_duration) {
      cli::cli_abort(
        "Expected `response_duration` to be a single positive integer-like"
      )
    }
  }

  #### Prepare initial state and parameters ####
  initial_state <- make_initial_state(country, initial_state_manual)

  # prepare susceptibility matrix for vaccination
  susc <- make_susc_matrix(vaccine_investment, country)

  parameters <- c(
    prepare_parameters.daedalus_country(country),
    prepare_parameters.daedalus_infection(infection),
    prepare_parameters.daedalus_vaccination(vaccine_investment),
    list(
      beta = get_beta(infection, country),
      susc = susc,
      openness = openness,
      response_time = response_time,
      response_duration = response_duration
    )
  )

  # filter out NULLs so missing values can be read as NAN in C++
  parameters <- drop_null(parameters)

  output <- daedalus_internal(
    time_end,
    parameters,
    initial_state,
    flags,
    ode_control,
    n_groups = 1
  )
  output_data <- prepare_output(output$data, country)

  # NOTE: needs to be compatible with `<daedalus_output>`
  # or equivalent from `{daedalus.compare}`
  output <- list(
    total_time = time_end,
    model_data = output_data,
    country_parameters = unclass(country),
    infection_parameters = unclass(infection), # infection is list
    response_data = list(
      response_strategy = response_strategy,
      openness = openness,
      closure_info = get_daedalus_response_times(
        output$event_data[[1]],
        time_end
      )
    )
  )
  as_daedalus_output(output)
}
