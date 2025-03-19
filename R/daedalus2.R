#' Initial values for model flags
#'
#' @return A vector of initial flag values; all flags are initially set to 'off'
#' or 0.0.
#'
initial_flags <- function() {
  vax_flag <- 0.0
  resp_flag <- 0.0
  growth_flag <- 0.0

  c(growth_flag, resp_flag, vax_flag)
}

#' Internal function for daedalus2
#'
#' @return A list of state values as returned by `dust2::dust_unpack_state()`.
#' @keywords internal
daedalus2_internal <- function(time_end, params, state) {
  # NOTE: sys params assumed suitable for `do.call()`
  sys_params <- list(daedalus_ode, pars = params)
  sys <- do.call(dust2::dust_system_create, sys_params)

  # convert state to vector and add initial flags
  state <- as.vector(state)
  state <- c(state, initial_flags())
  dust2::dust_system_set_state(sys, state)

  state <- dust2::dust_system_simulate(sys, seq(0, time_end))
  dust2::dust_unpack_state(sys, state)
}

#' DAEDALUS model implemented with dust
#'
#' `daedalus::daedalus2()` will eventually replace `daedalus::daedalus()` with
#' the model implemented in C++ to integrate with \pkg{dust2}
#' (and in future \pkg{dust}). *This is a work in progress!*
#'
#' @inheritParams daedalus
#'
#' @details
#' **Note that** `daedalus2()` currently uses a default vaccination strategy of
#' _no vaccination_, using the internal helper function [dummy_vaccination()].
#' This is expected to become the default for [daedalus()] when it is replaced
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
  time_end = 100
) {
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

  # checks on interventions
  # also prepare the appropriate economic openness vectors
  # allowing for a numeric vector, or NULL for no response
  if (is.null(response_strategy)) {
    openness <- rep(1.0, N_ECON_SECTORS)
  } else if (is.numeric(response_strategy)) {
    checkmate::assert_numeric(
      response_strategy,
      lower = 0,
      upper = 1,
      len = N_ECON_SECTORS
    )
    openness <- response_strategy
  } else if (response_strategy %in% names(daedalus::closure_data)) {
    openness <- daedalus::closure_data[[response_strategy]]
  }

  # checks on vaccination
  checkmate::assert_multi_class(
    vaccine_investment,
    c("daedalus_vaccination", "character"),
    null.ok = TRUE
  )
  if (is.null(vaccine_investment)) {
    vaccine_investment <- dummy_vaccination()
  }
  if (is.character(vaccine_investment)) {
    vaccine_investment <- rlang::arg_match(
      vaccine_investment,
      daedalus::vaccination_scenario_names
    )
    vaccine_investment <- daedalus_vaccination(vaccine_investment)
  }

  is_good_time_end <- checkmate::test_count(time_end, positive = TRUE)
  if (!is_good_time_end) {
    cli::cli_abort(c(
      "Expected `time_end` to be a single positive integer-like number.",
      i = "E.g. `time_end = 100`, but not `time_end = 100.5`"
    ))
  }

  #### Prepare initial state and parameters ####
  initial_state <- make_initial_state2(country)

  # prepare susceptibility matrix for vaccination
  susc <- make_susc_matrix(vaccine_investment, country)

  parameters <- c(
    prepare_parameters2.daedalus_country(country),
    prepare_parameters.daedalus_infection(infection),
    prepare_parameters2.daedalus_vaccination(vaccine_investment),
    list(
      beta = get_beta(infection, country),
      susc = susc,
      openness = openness
    )
  )

  output <- daedalus2_internal(time_end, parameters, initial_state)

  # NOTE: needs to be compatible with `<daedalus_output>`
  # or equivalent from `{daedalus.compare}`
  output
}
