#' Make large contact matrix for Cpp model.
#'
#' @name prepare_contacts
#' @rdname prepare_contacts
#'
#' @param country A `<daedalus_country>`.
#'
#' @keywords internal
#' @return
#'
#' 1. `make_conmat_large()` returns a 49x49 contact matrix scaled by the size of
#' demography groups.
#'
#' 2. `make_work_contacts()` returns a 45-element vector (for the number of
#' economic sectors) scaled by the number of workers per sector.
#'
#' 3. `make_consumer_contacts()` returns a 45x4 contact matrix with each row
#' scaled by the number of workers per sector. Dimensions are the number of
#' economic sectors and the number of age groups.
make_conmat_large <- function(country) {
  cm_nrow <- N_AGE_GROUPS + N_ECON_SECTORS

  cm <- matrix(NA, cm_nrow, cm_nrow)
  cm[i_AGE_GROUPS, i_AGE_GROUPS] <- country$contact_matrix
  cm[i_AGE_GROUPS, i_ECON_SECTORS] <- matrix(
    cm[i_AGE_GROUPS, i_WORKING_AGE],
    N_AGE_GROUPS,
    N_ECON_SECTORS
  )
  cm[i_ECON_SECTORS, i_AGE_GROUPS] <- matrix(
    cm[i_WORKING_AGE, i_AGE_GROUPS],
    N_ECON_SECTORS,
    N_AGE_GROUPS,
    byrow = TRUE
  )

  cm[is.na(cm)] <- cm[i_WORKING_AGE, i_WORKING_AGE]

  demog <- rep(country$demography[i_WORKING_AGE], cm_nrow)
  demog[i_AGE_GROUPS] <- country$demography
  cm <- cm / demog

  cm
}

#' @name prepare_contacts
make_work_contacts <- function(country) {
  country$contacts_workplace / country$workers
}

#' @name prepare_contacts
make_consumer_contacts <- function(country) {
  # col-wise divison by demography
  country$contacts_consumer_worker %*% diag(1 / country$demography)
}

#' @title Generate a default initial state for DAEDALUS
#' @description Function to prepare the model initial state. Assumes that
#' 1 in every million individuals is initially infected, and that 60% are
#' asymptomatic infections. This does not affect the actual probability of
#' asymptomatic infections in the simulation, which is a property of a
#' `<daedalus_infection>`.
#'
#' @inheritParams daedalus
#'
#' @return An array with as many dimensions as `N_VACCINE_DATA_GROUPS`
#' (currently, 3); rows specify the age and economic groups, columns specify the
#' epidemiological compartments (including new infections and hospitalisations),
#' and array layers hold information on vaccination status (including new
#' vaccinations).
#' @keywords internal
make_initial_state <- function(country, initial_state_manual) {
  # NOTE: country checked in daedalus2()
  initial_infect_state <- list(p_infectious = 1e-6, p_asymptomatic = 0.0)

  if (is.null(initial_state_manual)) {
    p_infectious <- initial_infect_state[["p_infectious"]]
    p_asymptomatic <- initial_infect_state[["p_asymptomatic"]]
  } else {
    initial_infect_state[names(initial_state_manual)] <- initial_state_manual

    p_infectious <- initial_infect_state[["p_infectious"]]
    p_asymptomatic <- initial_infect_state[["p_asymptomatic"]]

    # NOTE: no checks on country as this is tested in top-level fn `daedalus2()`
    # check other inputs
    is_good_p_infectious <- checkmate::test_number(
      p_infectious,
      lower = 0.0,
      upper = 1.0
    )
    if (!is_good_p_infectious) {
      cli::cli_abort(
        "`p_infectious` must be a single number in the range [0.0, 1.0].",
        .envir = parent.frame()
      )
    }

    is_good_p_asymp <- checkmate::test_number(
      p_asymptomatic,
      lower = 0.0,
      upper = 1.0
    )
    if (!is_good_p_asymp) {
      cli::cli_abort(
        "`p_asymptomatic` must be a single number in the range [0.0, 1.0]."
      )
    }
  }

  initial_state <- c(
    S = 1.0 - p_infectious,
    E = 0.0,
    Is = p_infectious * (1.0 - p_asymptomatic),
    Ia = p_infectious * p_asymptomatic,
    H = 0.0,
    R = 0.0,
    D = 0.0,
    dE = 0.0,
    dH = 0.0
  )

  # build for all age groups and economic sectors (working age only)
  initial_state <- matrix(
    initial_state,
    N_AGE_GROUPS + N_ECON_SECTORS,
    N_MODEL_COMPARTMENTS,
    byrow = TRUE
  )

  # get demography and sector workforce, including non-working
  demography <- get_data(country, "demography")
  sector_workforce <- get_data(country, "workers")
  inactive_workers <- demography[i_WORKING_AGE] - sum(sector_workforce)
  demography[i_WORKING_AGE] <- inactive_workers

  # multiply by demography and sector workforce, row-wise
  initial_state[seq_len(N_AGE_GROUPS), ] <- initial_state[
    seq_len(N_AGE_GROUPS),
  ] *
    demography

  initial_state[(N_AGE_GROUPS + 1):nrow(initial_state), ] <- initial_state[
    (N_AGE_GROUPS + 1):nrow(initial_state),
  ] *
    sector_workforce

  # add strata for vaccination groups and set to zero
  initial_state <- array(
    initial_state,
    c(dim(initial_state), N_VACCINE_DATA_GROUPS)
  )
  initial_state[,, -i_UNVACCINATED_STRATUM] <- 0

  initial_state
}

#' @title Generate initial state for daedalus2()
#'
#' @inheritParams make_initial_state
#' @keywords internal
make_initial_state2 <- function(
  country,
  initial_state_manual = list(p_infectious = 1e-7)
) {
  initial_state <- make_initial_state(country, initial_state_manual)

  initial_state[,, c(i_UNVACCINATED_STRATUM, i_VACCINATED_STRATUM)]
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
#' - `hosp_switch`: The switch for excess mortality due to more hospitalisations
#' required than hospital places are available.
#'
#' - `vax_switch`: A switch for whether vaccination is active. Manually switched
#' on in model stage 3.
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
    hosp_switch = FALSE,
    vax_switch = FALSE
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
    mutables,
    c("closure_time_start", "closure_time_end")
  )
  closure_times[["closure_duration"]] <- unname(diff(unlist(closure_times)))

  lapply(closure_times, floor)
}

#' Reshape a vector to the dimensions of the DAEDALUS state array
#'
#' @param x A vector of numeric values.
#'
#' @return An array of dimensions (4, 9, 46, 3).
#' @keywords internal
values_to_state <- function(x) {
  dim(x) <- c(
    N_AGE_GROUPS + N_ECON_SECTORS,
    N_MODEL_COMPARTMENTS,
    N_VACCINE_DATA_GROUPS
  )

  x
}
