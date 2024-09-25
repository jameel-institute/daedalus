#' Scale vaccination rate by remaining eligibles
#'
#' @param state The system state as a 4-dimensional array.
#' @param nu The vaccination rate.
#'
#' @return The scaled vaccination rate.
scale_nu <- function(state, nu) {
  total <- sum(state)
  eligible <- state[, c(i_S, i_R), , i_UNVACCINATED_STRATUM]

  # NOTE: simplified scaling works only for uniform rates and start times
  # across age groups!
  scaling <- total / sum(eligible)

  # handle zero division if there are no eligibles
  scaling <- if (is.nan(scaling) || is.infinite(scaling)) 0.0 else scaling

  nu_current <- scaling * nu

  # prevent more vaccinations than available individuals
  nu_current <- if (nu_current > 1.0) 1.0 else nu_current

  nu_current
}
