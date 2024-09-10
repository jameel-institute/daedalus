#' @title Event and root-finding functions to trigger response strategies
#' @description Prepare functions that can be passed to [deSolve::lsoda()] as
#' event and root-finding functions, which trigger response strategies that
#' reduce disease transmission.
#' @inheritParams daedalus
#' @keywords internal
make_response_threshold_event <- function(response_threshold) {
  # NOTE: input checking at top level
  ## event triggered when thresholds are crossed
  root_function <- function(time, state, parameters) {
    state <- array(
      state,
      c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_STRATA)
    )

    get_hospitalisations(state) - response_threshold
  }

  event_function <- function(time, state, parameters) {
    # prevent flipping switch when checkEventFunc runs
    if (time != parameters[["min_time"]]) {
      rlang::env_poke(parameters[["mutables"]], "switch", 1.0)
    }
    state
  }

  list(root_function = root_function, event_function = event_function)
}

#' @title Event and root-finding functions to terminate epidemic responses
#' @description Prepare functions that can be passed to [deSolve::lsoda()] as
#' event and root-finding functions, which trigger and end interventions that
#' scale disease transmission.
#' @keywords internal
make_rt_end_event <- function() {
  # NOTE: state reconstruction could be sped up
  root_function <- function(time, state, parameters) {
    state <- array(
      state,
      c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_STRATA)
    )

    # because contacts are divided by demography during parameter prep
    cm <- parameters[["contact_matrix"]] %*% diag(parameters[["demography"]])

    # arbitrary precision, may not be hit!
    r_eff(parameters[["r0"]], state, cm) - 0.99
  }

  event_function <- function(time, state, parameters) {
    # prevent flipping switch when checkEventFunc runs
    if (time != parameters[["min_time"]]) {
      rlang::env_poke(parameters[["mutables"]], "switch", 0.0)
    }
    state
  }

  list(root_function = root_function, event_function = event_function)
}
