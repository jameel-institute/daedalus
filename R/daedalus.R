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
  checkmate::assert_matrix(
    initial_state, "numeric",
    nrows = N_AGE_GROUPS, ncols = N_EPI_COMPARTMENTS,
    any.missing = FALSE
  )
  checkmate::assert_count(time_end, positive = TRUE)
  checkmate::assert_list(parameters, types = "numeric")

  data <- deSolve::lsoda(
    y = initial_state, times = seq.int(time_end),
    func = daedalus_rhs, parms = parameters
  )

  data
}
