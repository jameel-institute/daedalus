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

#' Calculate transmission parameter from infection and contact parameters
#'
#' @param infection A `<daedalus_infection>` object.
#'
#' @param country A `<daedalus_country>` object.
#'
#' @return A single numeric value for the transmission parameter, denoted
#' \eqn{\beta}.
#' @keywords internal
get_beta <- function(infection, country) {
  # NOTE: this calculation does not take workplace contacts into account

  r0 <- infection$r0
  cm <- country$contact_matrix
  nn <- country$demography
  eta <- infection$eta
  sigma <- infection$sigma
  p_sigma <- infection$p_sigma
  epsilon <- infection$epsilon
  gamma_Ia <- infection$gamma_Ia
  gamma_Is <- infection$gamma_Is

  sig1 <- sigma * (1 - p_sigma)
  sig2 <- sigma * p_sigma
  red <- epsilon

  FOIa <- red * cm
  FOIs <- cm

  Fmat <- matrix(
    0,
    N_INFECTION_SUBSYSTEM * N_AGE_GROUPS,
    N_INFECTION_SUBSYSTEM * N_AGE_GROUPS
  )

  # assign the F_matrix elements: hardcoding numbers for now
  # i_AGE_GROUPS + N_AGE_GROUPS: infectious_asymptomatic compartments
  # i_AGE_GROUPS + N_AGE_GROUPS * 2: infectious symptomatic compartments
  Fmat[i_AGE_GROUPS, i_AGE_GROUPS + N_AGE_GROUPS] <- FOIa
  Fmat[i_AGE_GROUPS, i_AGE_GROUPS + N_AGE_GROUPS * 2] <- FOIs

  ones <- matrix(1, N_AGE_GROUPS)
  vvec <- c(sigma * ones, gamma_Ia * ones, gamma_Is * ones)
  # this assumes equal duration infectious to recovery and hospitalisation

  Vmat <- diag(vvec)

  Vmat[i_AGE_GROUPS + N_AGE_GROUPS, i_AGE_GROUPS] <- diag(-sig1 * ones)
  Vmat[i_AGE_GROUPS + N_AGE_GROUPS * 2, i_AGE_GROUPS] <- diag(-sig2 * ones)

  NGM <- Fmat %*% solve(Vmat)
  R0a <- max(Re(eigen(NGM)$values))

  beta <- r0 / R0a
  beta
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
