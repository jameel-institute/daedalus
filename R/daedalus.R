#' @title DAEDALUS model for health, social and economic costs of a pandemic
#'
#' @description Run the DAEDALUS model from R. This is a work in progress.
#'
#' @param country A country or territory object of class `<daedalus_country>`,
#' **or** a country or territory name from those included in the package;
#' see [daedalus::country_names].
#' Country-specific data such as the community and workplace contacts, the
#' demography, and the distribution of the workforce into economic sectors is
#' automatically accessed from package data for the relevant country name if it
#' is passed as a string.
#' To override package defaults for country characteristics, pass a
#' `<daedalus_country>` object instead. See [daedalus_country()] for more.
#'
#' @param infection An infection parameter object of the class `<infection>`,
#' **or** an epidemic name for which data are provided in the package; see
#' [daedalus::epidemic_names] for parameters from a historical epidemic
#' or epidemic wave.
#' Passing the name as a string automatically accesses the default parameters
#' of an infection. Create an pass a `<daedalus_infection>` to tweak infection
#' parameters.
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
#' @return A `<deSolve>` object.
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
#' @export
daedalus <- function(country,
                     infection,
                     response_strategy = c(
                       "none", "elimination", "economic_closures",
                       "school_closures"
                     ),
                     implementation_level = c("light", "heavy"),
                     response_time = 30,
                     response_threshold = 1000,
                     initial_state_manual = list(),
                     time_end = 300) {
  # input checking
  # NOTE: names are case sensitive
  checkmate::assert_multi_class(country, c("daedalus_country", "character"))
  if (is.character(country)) {
    country <- rlang::arg_match(country, daedalus::country_names)
    country <- daedalus_country(country)
  }
  checkmate::assert_multi_class(infection, c("daedalus_infection", "character"))
  if (is.character(infection)) {
    infection <- rlang::arg_match(infection, daedalus::epidemic_names)
    infection <- daedalus_infection(infection)
  }

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
    prepare_parameters(country),
    prepare_parameters(infection)
  )

  # NOTE: using {rlang} for convenience
  mutables <- prepare_mutable_parameters()

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

  # set switch parameter and log closure start time if not 0.0
  rlang::env_poke(parameters[["mutables"]], "switch", 1.0)

  is_response_active <- as.logical(
    rlang::env_get(parameters[["mutables"]], "closure_time_start")
  ) # coerce to logical; automatically FALSE as default value is 0.0

  if (!is_response_active) {
    rlang::env_poke(
      parameters[["mutables"]], "closure_time_start", response_time
    )
  }

  # reset min time
  parameters[["min_time"]] <- response_time

  data_stage_two <- deSolve::lsodar(
    initial_state, times_stage_two,
    daedalus_rhs, parameters,
    rootfunc = termination_event[["root_function"]],
    events = list(func = termination_event[["event_function"]], root = TRUE)
  )

  data <- rbind(data_stage_one, data_stage_two[-1, ])

  # NOTE: unclassing country and infection returns lists
  data <- list(
    model_data = prepare_output(data),
    country_parameters = unclass(country),
    infection_parameters = unclass(infection),
    response_data = list(
      response_strategy = response_strategy,
      implementation_level = implementation_level,
      closure_info = get_closure_info(mutables)
    )
  )

  as_daedalus_output(data)
}
