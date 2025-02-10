#' Internal function for daedalus2
#'
#' @return A list of state values as returned by `dust2::dust_unpack_state()`.
#' @keywords internal
daedalus2_internal <- function(time_end, params, state) {
  # NOTE: sys params assumed suitable for `do.call()`
  sys_params <- list(daedalus_ode, pars = params)
  sys <- do.call(
    dust2::dust_system_create, sys_params
  )

  dust2::dust_system_set_state(sys, as.vector(state))

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
#' @export
#'
#' @examples
#' daedalus2("GBR", "sars_cov_1")
daedalus2 <- function(
    country, infection,
    time_end = 100) {
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

  #### Prepare initial state and parameters ####
  initial_state <- make_initial_state2(country)

  parameters <- c(
    prepare_parameters2.daedalus_country(country),
    prepare_parameters.daedalus_infection(infection),
    list(
      beta = get_beta(infection, country)
    )
  )

  output <- daedalus2_internal(time_end, parameters, initial_state)

  # NOTE: needs to be compatible with `<daedalus_output>`
  # or equivalent from `{daedalus.compare}`
  output
}
