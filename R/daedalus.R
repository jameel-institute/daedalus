#' @title DAEDALUS model for health, social and economic costs of a pandemic
#'
#' @description Run the DAEDALUS model from R. This is a work in progress.
#' @param initial_state The initial state of the population as a numeric array
#' where rows represent age groups, columns represent epidemiological
#' compartments, and layers represent economic sectors.
#' @param time_end An integer-like value for the number of timesteps
#' at which to return data. This is treated as the number of days with data
#' returned for each day. Defaults to 300 days.
#' @param parameters A list of model parameters. See [default_parameters()] for
#' model defaults.
#'
#' @return A `<deSolve>` object.
#' @export
daedalus <- function(initial_state, time_end = 300L,
                     parameters = default_parameters()) {
  # NOTE: see constants.R for package constants
  is_array_numeric <- checkmate::test_array(
    initial_state, "numeric",
    any.missing = FALSE
  )
  if (!is_array_numeric) {
    cli::cli_abort(
      "Expected `initial_state` to be a numeric {.cls {'array'}} with no
      missing elements but it is {.obj_type_friendly {initial_state}} or has
      missing elements."
    )
  }

  is_good_array <- checkmate::test_array(
    initial_state,
    d = 3L
  )
  if (!is_good_array) {
    cli::cli_abort(
      "Expected `initial_state` to be a numeric {.cls {'array'}} with three
      dimensions, but it has {length(dim(initial_state))} dimensions."
    )
  }

  has_correct_dims <- identical(
    dim(initial_state),
    c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_SECTORS)
  )
  if (!has_correct_dims) {
    cli::cli_abort(
      c(
        "Expected `initial_state` to have dimensions
        {c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_SECTORS)},
        but it has dimensions {dim(initial_state)}.",
        i = "The number of rows in `initial_state` represents the number of
        age groups, the number of columns represents the number of
        epidemiological compartments, and the lenght of the third dimension
        represents the number of economic sectors."
      )
    )
  }

  is_good_time_end <- checkmate::test_count(time_end, positive = TRUE)
  if (!is_good_time_end) {
    cli::cli_abort(
      "Expected `time_end` to be a single positive number."
    )
  }
  is_numeric_list <- checkmate::test_list(
    parameters,
    types = "numeric", any.missing = FALSE, all.missing = FALSE,
    min.len = length(default_parameters())
  )
  if (!is_numeric_list) {
    cli::cli_abort(
      c(
        "Expected `parameters` to be a list with only numeric elements (single
        numeric values or matrices).",
        i = "See {.fn daedalus::default_parameters} for allowed parameter names
        and default values."
      )
    )
  }

  data <- deSolve::lsoda(
    y = initial_state, times = seq.int(time_end),
    func = daedalus_rhs, parms = parameters
  )

  data
}
