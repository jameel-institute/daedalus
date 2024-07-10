#' DAEDALUS model for health, social and economic costs of a pandemic
#'
#' @param initial_state The initial state of the population as a numeric matrix
#' where rows represent age groups and columns represent epidemiological
#' compartments.
#' @param time_end An integer-like value (e.g. 100) for the number of timesteps
#' at which to return data. This is treated as the number of days with data
#' returned for each day.
#' @param parameters A list of model parameters. See [default_parameters()] for
#' model defaults.
#'
#' @return A `<deSolve>` object.
#' @export
daedalus <- function(initial_state, time_end = 100L,
                     parameters = default_parameters()) {
  # input checking
  # check the initial state matrix for a hardocded number of age groups
  # and epidemiomological compartments
  checkmate::assert_matrix(
    initial_state, "numeric",
    nrows = 4L, ncols = 7L, any.missing = FALSE
  )
  checkmate::assert_count(time_end, positive = TRUE)
  # check parameters are numeric inputs, passes for numeric matrices
  checkmate::assert_list(parameters, types = "numeric")

  # run model and return data as <deSolve> object
  data <- deSolve::lsoda(
    y = initial_state, times = seq.int(time_end),
    func = .daedalus_ode, parms = parameters
  )

  # return data
  data
}
