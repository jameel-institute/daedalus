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
  cm <- make_full_contacts(country)
  n_demog <- N_AGE_GROUPS

  r0 <- infection$r0
  sigma <- infection$sigma
  p_sigma <- infection$p_sigma
  epsilon <- infection$epsilon
  gamma_Ia <- infection$gamma_Ia
  gamma_Is <- infection$gamma_Is

  sig1 <- sigma * (1 - p_sigma)
  sig2 <- sigma * p_sigma

  foi_a <- epsilon * cm
  foi_s <- cm

  # get the F matrix for the NGM with small domain
  # which relates only to the state at infection E
  f_mat <- cbind(
    matrix(0.0, n_demog, n_demog),
    foi_a,
    foi_s
  )

  vvec <- c(
    rep(sigma, n_demog),
    rep(gamma_Ia, n_demog),
    rep(gamma_Is, n_demog)
  )
  # this assumes equal duration infectious to recovery and hospitalisation
  # is this also assuming isToH never contribute to FoI?

  v_mat <- diag(vvec)

  v_mat[seq(n_demog) + n_demog, seq(n_demog)] <- diag(
    rep(-sig1, n_demog)
  )
  v_mat[seq(n_demog) + n_demog * 2, seq(n_demog)] <- diag(
    rep(-sig2, n_demog)
  )

  v_inv <- solve(v_mat)

  ngm <- f_mat %*% v_inv[, seq(n_demog)]

  R0a <- max(Re(eigen(ngm)$values))

  beta <- r0 / R0a
  beta
}

#' @title Calculate the effective R
#'
#' @name reff_calculation
#' @rdname reff_calculation
#'
#' @description
#' A simple calculation for the next-generation matrix, allowing for
#' heterogeneity in social contacts and optionally, a reduction in the
#' proportion of each age group remaining susceptible.
#'
#' This assumes a single susceptible stratum with full susceptibility, and will
#' be updated in future versions to account for the addition of vaccination
#' strata with reduced susceptibility.
#'
#' @param country A `<daedalus_country>` object.
#'
#' @param infection A `<daedalus_infection>` object.
#'
#' @param p_susc A sequence of length 4 (the number of demographic groups)
#' giving the proportion of susceptibles in each demographic group.
#'
#' @return A numeric matrix of dimensions \eqn{[4, 4]} where the number of rows
#' and columns is the number of demographic groups.
#'
#' @details
#' Follows the methods in Diekmann and Hesterbeek (2010) J. Roy. Soc. Interface,
#' <doi.org/10.1098/rsif.2009.0386>.
#'
#'
#' @keywords internal
get_ngm <- function(country, infection, p_susc = NULL) {
  # internal fn with no input checks
  cm <- make_full_contacts(country)
  n_demog <- N_AGE_GROUPS
  if (is.null(p_susc)) {
    p_susc <- rep(1.0, n_demog)
  }
  cm_scaled <- cm * p_susc # could be placed at the end

  beta <- get_beta(infection, country)
  sigma <- infection$sigma
  p_sigma <- infection$p_sigma
  epsilon <- infection$epsilon
  gamma_Ia <- infection$gamma_Ia
  gamma_Is <- infection$gamma_Is

  sig1 <- sigma * (1 - p_sigma)
  sig2 <- sigma * p_sigma

  foi_a <- epsilon * beta * cm_scaled
  foi_s <- beta * cm_scaled

  # get the F matrix for the NGM with small domain
  # which relates only to the state at infection E
  f_mat <- cbind(
    matrix(0.0, n_demog, n_demog),
    foi_a,
    foi_s
  )

  vvec <- c(rep(sigma, n_demog), rep(gamma_Ia, n_demog), rep(gamma_Is, n_demog))
  # this assumes equal duration infectious to recovery and hospitalisation

  v_mat <- diag(vvec)

  v_mat[seq(n_demog) + n_demog, seq(n_demog)] <- diag(
    rep(-sig1, n_demog)
  )
  v_mat[seq(n_demog) + n_demog * 2, seq(n_demog)] <- diag(
    rep(-sig2, n_demog)
  )

  v_inv <- solve(v_mat)

  ngm <- f_mat %*% v_inv[, seq(n_demog)]

  ngm
}
