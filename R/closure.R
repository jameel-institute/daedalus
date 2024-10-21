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
    state <- values_to_state(state)

    get_hospitalisations(state) - response_threshold
  }

  event_function <- function(time, state, parameters) {
    # NOTE: prevent flipping switch when checkEventFunc runs
    # NOTE: prevent response activation if epdedemic is not growing
    state <- values_to_state(state)

    rt <- r_eff(parameters[["r0"]], state, parameters[["cm_unscaled"]])

    # NOTE: to ensure only first hosp threshold crossing is logged
    is_hosp_switch_on <- rlang::env_get(parameters[["mutables"]], "hosp_switch")

    if (time != parameters[["min_time"]] && !is_hosp_switch_on) {
      rlang::env_poke(
        parameters[["mutables"]], "hosp_switch", TRUE
      )

      # NOTE: trigger response and log closure start time only if
      # epidemic is growing
      if (rt >= 1.0) {
        rlang::env_bind(
          parameters[["mutables"]],
          switch = TRUE,
          closure_time_start = time
        )
      }
    }
    as.numeric(state)
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
    state <- values_to_state(state)

    # arbitrary precision, may not be hit!
    r_eff(parameters[["r0"]], state, parameters[["cm_unscaled"]]) - 0.99
  }

  event_function <- function(time, state, parameters) {
    # NOTE: log the FIRST time Rt < 1.0 as closure time end
    is_switch_on <- rlang::env_get(parameters[["mutables"]], "switch")
    # prevent flipping switch when checkEventFunc runs
    if (time != parameters[["min_time"]] && is_switch_on) {
      rlang::env_bind(
        parameters[["mutables"]],
        switch = FALSE,
        closure_time_end = time
      )
    }

    # switch execess mortality on or off independent of closure status
    if (time != parameters[["min_time"]]) {
      # check if hospitalisations are greater than threshold
      state <- values_to_state(state)
      total_hosp <- get_hospitalisations(state)

      if (total_hosp > parameters[["hospital_capacity"]]) {
        rlang::env_poke(
          parameters[["mutables"]], "hosp_switch", TRUE
        )
      } else {
        rlang::env_poke(
          parameters[["mutables"]], "hosp_switch", FALSE
        )
      }
    }

    as.numeric(state)
  }

  list(root_function = root_function, event_function = event_function)
}
