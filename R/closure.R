#' @title Event and root-finding functions for thresholded interventions
#' @description Prepare functions that can be passed to [deSolve::lsoda()] as
#' event and root-finding functions, which trigger and end interventions that
#' scale disease transmission.
#' @inheritParams daedalus
#' @keywords internal
make_closure_event <- function(response_threshold, end_threshold) {
  # NOTE: input checking at top level
  # get compartment names and values
  name_response_cmpt <- names(response_threshold)
  name_end_cmpt <- names(end_threshold)

  i_response_cmpt <- which(COMPARTMENTS == name_response_cmpt)
  i_end_cmpt <- which(COMPARTMENTS == name_end_cmpt)

  ## event triggered when thresholds are crossed
  root_function <- function(time, state, parameters) {
    state <- state[-length(state)]
    state <- array(
      state,
      c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_STRATA)
    )

    root <- c(
      sum(state[, i_response_cmpt, ]) - response_threshold,
      sum(state[, i_end_cmpt, ]) - end_threshold
    )
    root
  }

  event_function <- function(time, state, parameters) {
    state_ <- state[-length(state)]
    state_ <- array(
      state_,
      c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_STRATA)
    )

    root <- c(
      sum(state_[, i_response_cmpt, ]) - response_threshold,
      sum(state_[, i_end_cmpt, ]) - end_threshold
    )

    which_root <- which(abs(root) < 1e-6)

    if (length(which_root) == 0) which_root <- 0 # handle empty which() output
    state["switch"] <- if (which_root == 1) 1.0 else 0.0

    state
  }

  list(root_function = root_function, event_function = event_function)
}
