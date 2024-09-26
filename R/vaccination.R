#' Scale vaccination rate by remaining eligibles
#'
#' @param state The system state as a 4-dimensional array.
#' @param nu The vaccination rate.
#'
#' @return The scaled vaccination rate.
#' @keywords internal
scale_nu <- function(state, nu) {
  total <- sum(state)
  eligible <- state[, c(i_S, i_R), , i_UNVACCINATED_STRATUM]

  # NOTE: simplified scaling works only for uniform rates and start times
  # across age groups!
  scaling <- total / sum(eligible)

  # handle zero division if there are no eligibles
  scaling <- if (is.finite(scaling)) 0.0 else scaling

  # prevent more vaccinations than available individuals
  min(1.0, scaling * nu)
}
