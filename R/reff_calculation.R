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

  ones <- rep(1, N_AGE_GROUPS)
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
