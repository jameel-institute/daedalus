#' @title DAEDALUS model for health, social and economic costs of a pandemic
#'
#' @description Run the DAEDALUS model from R. This is a work in progress.
#' @param initial_state The initial state of the population as a numeric matrix
#' where rows represent age groups and columns represent epidemiological
#' compartments.
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
  is_good_matrix <- checkmate::test_matrix(
    initial_state, "numeric",
    nrows = N_AGE_GROUPS, ncols = N_EPI_COMPARTMENTS,
    any.missing = FALSE
  )
  if (!is_good_matrix) {
    cli::cli_abort(
      c(
        "Expected `initial_state` to be a numeric matrix with {N_AGE_GROUPS}
        rows and {N_EPI_COMPARTMENTS} columns with no missing values, but it has
        {nrow(initial_state)} rows and {ncol(initial_state)} columns.",
        i = "The number of rows in `initial_state` represents the number of
        age groups, while the number of columsn represents the number of
        epidemiological compartments."
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
