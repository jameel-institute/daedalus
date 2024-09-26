#' @title Generate a default initial state for DAEDALUS
#' @description Function to prepare the model initial state.
#'
#' @inheritParams daedalus
#'
#' @return An array with as many dimensions as `N_ECON_STRATA` (currently, 46)
#' with each layer giving the proportion of individuals of each group in each
#' epidemiological compartment.
#' @keywords internal
make_initial_state <- function(country, initial_state_manual) {
  # NOTE: country checked in daedalus()
  initial_infect_state <- list(
    p_infectious = 1e-6,
    p_asymptomatic = 0.0
  )
  initial_infect_state[names(initial_state_manual)] <- initial_state_manual

  p_infectious <- initial_infect_state[["p_infectious"]]
  p_asymptomatic <- initial_infect_state[["p_asymptomatic"]]

  # NOTE: no checks on country as this is tested in top-level fn `daedalus()`
  # check other inputs
  is_good_p_infectious <- checkmate::test_number(
    p_infectious,
    lower = 0.0, upper = 1.0
  )
  if (!is_good_p_infectious) {
    cli::cli_abort(
      "`p_infectious` must be a single number in the range [0.0, 1.0].",
      .envir = parent.frame()
    )
  }

  is_good_p_asymp <- checkmate::test_number(
    p_asymptomatic,
    lower = 0.0, upper = 1.0
  )
  if (!is_good_p_asymp) {
    cli::cli_abort(
      "`p_asymptomatic` must be a single number in the range [0.0, 1.0]."
    )
  }

  initial_state <- c(
    S = 1.0 - p_infectious, E = 0.0,
    Is = p_infectious * (1.0 - p_asymptomatic),
    Ia = p_infectious * p_asymptomatic,
    H = 0.0, R = 0.0, D = 0.0,
    dE = 0.0, dH = 0.0
  )

  # build for all age groups
  initial_state <- array(
    rep(initial_state, each = N_AGE_GROUPS),
    c(N_AGE_GROUPS, N_MODEL_COMPARTMENTS, N_ECON_STRATA, N_VACCINE_STRATA)
  )
  # set vaccinated to zero
  initial_state[, , , i_VACCINATED_STRATUM] <- 0.0

  # get demography and sector workforce
  demography <- get_data(country, "demography")
  sector_workforce <- get_data(country, "workers")

  # calculate non-working working-age, and prepare initial state accounting
  # for distribution of working age into economic sectors
  inactive_workers <- demography[i_WORKING_AGE] - sum(sector_workforce)

  initial_state[-i_WORKING_AGE, , i_NOT_WORKING, i_UNVACCINATED_STRATUM] <-
    initial_state[-i_WORKING_AGE, , i_NOT_WORKING, i_UNVACCINATED_STRATUM] *
      demography[-i_WORKING_AGE]

  initial_state[i_WORKING_AGE, , i_NOT_WORKING, i_UNVACCINATED_STRATUM] <-
    initial_state[i_WORKING_AGE, , i_NOT_WORKING, i_UNVACCINATED_STRATUM] *
      inactive_workers

  # explicit col-wise multiplication as R tries to guess interpretation of `*`
  initial_state[i_WORKING_AGE, , -i_NOT_WORKING, i_UNVACCINATED_STRATUM] <-
    initial_state[i_WORKING_AGE, , -i_NOT_WORKING, i_UNVACCINATED_STRATUM] %*%
    diag(sector_workforce)

  # set all economic sector non-working age values to 0
  initial_state[-i_WORKING_AGE, , -i_NOT_WORKING, ] <- 0.0

  initial_state
}

#' Prepare mutable parameters for the DAEDALUS model
#'
#' @description Prepares closure start and end times for model output.
#'
#' @return
#' An environment with three mutable parameters:
#'
#' - `switch`: The switch parameter which controls whether closures are active
#' or not.
#'
#' - `closures_time_start` and `closures_time_end`: The times at which closures
#' start and end. Defaults to the end time of the simulation so as to
#' give a default duration of 0.0.
#' @keywords internal
prepare_mutable_parameters <- function() {
  env <- rlang::env(
    switch = FALSE,

    # set closure time start and time end to 0.0
    # to later process duration as time_start - time_end
    closure_time_start = 0.0,
    closure_time_end = 0.0,
    hosp_switch = FALSE
  )

  env
}

#' Get closure time limits and calculate duration
#'
#' @param mutables An environment holding the mutable parameters.
#' See [prepare_mutable_parameters()] for a template.
#'
#' @return
#' A three-element list of the start time, end time, and duration for which
#' closures are active.
#' @keywords internal
get_closure_info <- function(mutables) {
  closure_times <- rlang::env_get_list(
    mutables, c("closure_time_start", "closure_time_end")
  )
  closure_times[["closure_duration"]] <- unname(diff(unlist(closure_times)))

  lapply(closure_times, floor)
}
