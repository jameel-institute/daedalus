#' @title DAEDALUS model for health, social and economic costs of a pandemic
#'
#' @description Run the DAEDALUS model from R. This is a work in progress.
#'
#' @param country A country or territory object of class `<daedalus_country>`,
#' **or** a country or territory name from those included in the package;
#' see [daedalus::country_names], **or** a country ISO2 or ISO3 code; see
#' [daedalus::country_codes_iso2c] and [daedalus::country_codes_iso3c] .
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
#' @param initial_state_manual An optional **named** list with the names
#' `p_infectious` and `p_asymptomatic` for the proportion of infectious and
#' symptomatic individuals in each age group and economic sector.
#' Defaults to `1e-6` and `0.0` respectively.
#'
#' @param time_end An integer-like value for the number of timesteps
#' at which to return data. This is treated as the number of days with data
#' returned for each day. Defaults to 300 days.
#'
#' @param ... Other arguments to be passed to the ODE solver; these are passed
#' to [deSolve::ode()].
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
daedalus_rtm <- function(country,
                         infection,
                         response_strategy = NULL,
                         response_time_start = 30,
                         response_time_end = 100,
                         initial_state_manual = list(),
                         time_end = 300,
                         ...) {
  # input checking
  # NOTE: names are case sensitive
  checkmate::assert_multi_class(country, c("daedalus_country", "character"))
  if (is.character(country)) {
    country <- daedalus_country(country)
  }
  checkmate::assert_multi_class(infection, c("daedalus_infection", "character"))
  if (is.character(infection)) {
    infection <- rlang::arg_match(infection, daedalus::epidemic_names)
    infection <- daedalus_infection(infection)
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

  is_good_response_time <- checkmate::test_integerish(
    response_time_start,
    upper = time_end - 2L, lower = 2L, any.missing = FALSE
  )
  if (!is_good_response_time) {
    cli::cli_abort(
      "Expected `response_time` to be between 2 and {time_end - 2L}."
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
  if (is.null(response_strategy)) {
    openness <- rep(1.0, N_ECON_SECTORS)
  } else if (is.numeric(response_strategy)) {
    openness <- response_strategy
  } else if (response_strategy %in% names(closure_data)) {
    openness <- daedalus::closure_data[[response_strategy]]
  }

  # NOTE: psi (vax waning rate), tau (vax reduction in suscept.), and dims of nu
  # are hard-coded until vaccination scenarios are decided
  parameters <- c(
    parameters,
    list(
      # to increase HFR if crossed
      hospital_capacity = get_data(country, "hospital_capacity"),
      psi = 1 / 270,
      tau = c(1.0, 0.5),
      beta = get_beta(infection, country),
      openness = openness,
      mutables = mutables,
      min_time = 1 # setting minimum time to prevent switch flipping
    )
  )

  times <- seq_len(time_end)

  # ode solving
  data <- deSolve::ode(
    y = initial_state, times = times,
    func = daedalus_rhs, parms = parameters,
    ...
  )

  # NOTE: unclassing country and infection returns lists
  data <- list(
    total_time = time_end,
    model_data = prepare_output(data),
    country_parameters = unclass(country),
    infection_parameters = unclass(infection),
    response_data = list(
      response_strategy = response_strategy,
      openness = openness # easier to include here
    )
  )

  as_daedalus_output(data)
}
