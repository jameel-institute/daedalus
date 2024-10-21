#' @title Calculate the effective R
#'
#' @name reff_calculation
#' @rdname reff_calculation
#'
#' @description
#' A simple calculation for the effective \eqn{R_\text{eff}}, allowing for
#' heterogeneity in social contacts and the proportion of each age group
#' remaining susceptible.
#' This assumes a single susceptible stratum with full susceptibility, and will
#' be updated in future versions to account for the addition of vaccination
#' strata with reduced susceptibility.
#' @param r0 The basic reproduction number.
#' @param state The ODE state variable. Must be an array.
#' @param cm The contact matrix.
#'
#' @return A single numeric value for the \eqn{R_\text{eff}}.
#'
#' @keywords internal
r_eff <- function(r0, state, cm) {
  # NOTE: assumes state is a 4D array, and
  # cm is a 2D contact matrix with eigenvalue = 1.0
  # NOTE: reduced susceptibility for vaccinated!
  p_susc <- sum(
    state[, i_S, i_UNVACCINATED_STRATUM] +
      0.5 * state[, i_S, i_VACCINATED_STRATUM]
  ) / sum(state[, i_EPI_COMPARTMENTS, -i_NEW_VAX_STRATUM])

  r0 * p_susc
}

#' @title Calculate total hospitalisations
#'
#' @param state The ODE state variable as an array. Must have at least `i_H`
#' columns.
#' @keywords internal
get_hospitalisations <- function(state) {
  # NOTE: assumes state is a 4D array; not checked as this is internal
  # remove the new vaccinations stratum from sum
  sum(state[, i_H, -i_NEW_VAX_STRATUM])
}
