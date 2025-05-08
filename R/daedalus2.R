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
#' @param output dust2 output from `daedalus_internal()`.
#' @param time_end The model end time, passed from [daedalus2()].
#'
#' @return A vector of event start and end times suitable for a
#' `<daedalus_output>` object. Returns model end time if there is no response
#' end time.
#'
#' @keywords internal
get_daedalus2_response_times <- function(output, time_end) {
  # internal function with no input checking
  event_data <- output$event_data

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

#' Internal function for daedalus2
#'
#' @return A list of state values as returned by `dust2::dust_unpack_state()`.
#' @keywords internal
daedalus2_internal <- function(time_end, params, state, flags, ode_control) {
  # NOTE: sys params assumed suitable for `do.call()`
  arg_list <- list(
    generator = daedalus_ode,
    pars = params,
    ode_control = ode_control
  )
  sys <- do.call(dust2::dust_system_create, arg_list)

  # add initial flags
  state <- c(state, flags)
  dust2::dust_system_set_state(sys, state)

  state <- dust2::dust_system_simulate(sys, seq(0, time_end))

  list(
    data = dust2::dust_unpack_state(sys, state),
    event_data = dust2::dust_system_internals(sys)[["events"]][[1]]
  )
}

#' DAEDALUS model implemented with dust
#'
#' `daedalus::daedalus2()` will eventually replace `daedalus::daedalus2()` with
#' the model implemented in C++ to integrate with \pkg{dust2}
#' (and in future \pkg{dust}). *This is a work in progress!*
#'
#' @inheritParams daedalus
#'
#' @param response_duration A single integer-ish number that gives the number of
#' days after `response_time` that an NPI should end.
#'
#' @param ... Optional arguments that are passed to [dust2::dust_ode_control()].
#'
#' @details
#' **Note that** `daedalus2()` currently uses a default vaccination strategy of
#' _no vaccination_, using the internal helper function [dummy_vaccination()].
#' This is expected to become the default for [daedalus2()] when it is replaced
#' with `daedalus2()`.
#'
#' **Note also** that `daedalus2()` vaccination begins at the model start time,
#' and not at the time specified in `vaccine_investment`. This functionality
#' will be updated soon.
#'
#' @export
#'
#' @examples
#' # not yet ready for conversion to `<daedalus_output>`
#' output <- daedalus2("GBR", "sars_cov_1")
#'
#' names(output)
daedalus2 <- function(
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
  # NOTE: names are case sensitive
  checkmate::assert_multi_class(country, c("daedalus_country", "character"))
  if (is.character(country)) {
    country <- daedalus_country(country)
  }
  checkmate::assert_multi_class(infection, c("daedalus_infection", "character"))
  if (is.character(infection)) {
    infection <- rlang::arg_match(infection, daedalus.data::epidemic_names)
    infection <- daedalus_infection(infection)
  }

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
  checkmate::assert_multi_class(
    vaccine_investment,
    c("daedalus_vaccination", "character"),
    null.ok = TRUE
  )
  if (
    is_daedalus_vaccination(vaccine_investment) &&
      get_data(vaccine_investment, "start_time") == 0.0
  ) {
    # check vaccination start time and set vaccination flag
    flags["vax_flag"] <- 1.0
  }
  if (is.null(vaccine_investment)) {
    vaccine_investment <- dummy_vaccination()
  }
  if (is.character(vaccine_investment)) {
    vaccine_investment <- rlang::arg_match(
      vaccine_investment,
      daedalus.data::vaccination_scenario_names
    )
    vaccine_investment <- daedalus_vaccination(vaccine_investment)
  }

  # NULL converted to "none"; WIP: this will be moved to a class constructor
  if (response_strategy != "none") {
    is_good_response_time <- checkmate::test_integerish(
      response_time,
      upper = time_end - 2L, # for compat with daedalus
      lower = 1L, # responses cannot start at 0, unless strategy is null
      any.missing = FALSE,
      len = 1
    )
    if (!is_good_response_time) {
      cli::cli_abort(
        "Expected `response_time` to be between 1 and {time_end - 2L}."
      )
    }

    is_good_response_duration <- checkmate::test_integerish(
      response_duration,
      lower = 0L, # no minimum duration
      any.missing = FALSE,
      len = 1
    )
    if (!is_good_response_duration) {
      cli::cli_abort(
        "Expected `response_duration` to be a single integer-like and >= 0"
      )
    }
  }

  is_good_time_end <- checkmate::test_count(time_end, positive = TRUE)
  if (!is_good_time_end) {
    cli::cli_abort(c(
      "Expected `time_end` to be a single positive integer-like number.",
      i = "E.g. `time_end = 100`, but not `time_end = 100.5`"
    ))
  }

  #### Prepare initial state and parameters ####
  initial_state <- as.vector(make_initial_state2(country, initial_state_manual))

  # add state for new vaccinations by age group and econ sector
  state_new_vax <- numeric(
    length(get_data(country, "demography")) +
      length(get_data(country, "workers"))
  )
  initial_state <- c(
    initial_state,
    state_new_vax
  )

  # prepare susceptibility matrix for vaccination
  susc <- make_susc_matrix(vaccine_investment, country)

  parameters <- c(
    prepare_parameters2.daedalus_country(country),
    prepare_parameters.daedalus_infection(infection),
    prepare_parameters2.daedalus_vaccination(vaccine_investment),
    list(
      beta = get_beta(infection, country),
      susc = susc,
      openness = openness,
      response_time = response_time,
      response_duration = response_duration
    )
  )

  # filter out NULLs so missing values can be read as NAN in C++
  parameters <- Filter(function(x) !is.null(x), parameters)

  output <- daedalus2_internal(
    time_end,
    parameters,
    initial_state,
    flags,
    ode_control
  )

  # NOTE: needs to be compatible with `<daedalus_output>`
  # or equivalent from `{daedalus.compare}`
  output <- list(
    total_time = time_end,
    model_data = prepare_output_cpp(output$data, country),
    country_parameters = unclass(country),
    infection_parameters = unclass(infection),
    response_data = list(
      response_strategy = response_strategy,
      openness = openness,
      closure_info = get_daedalus2_response_times(output, time_end)
    )
  )

  as_daedalus_output(output)
}
