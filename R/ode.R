#' @title DAEDALUS system of ODEs
#' @description A compartmental epidemiological model with four age groups. This
#' function is intended to be passed to the solvers from the \pkg{deSolve}
#' package. This is a work in progress and additional components will be added
#' in future.
#' @param t A single double value for the time.
#' @param state Vector of the initial state. The form of the vector should be
#' \eqn{X_i, for X \in S, E, I_s, I_a, R, H, D}, where values for each
#' of the compartments for each age group \eqn{i} are given consecutively.
#' May be a matrix to allow for multiple age groups. See [daedalus()] for an
#' explanation of the epidemiological compartments.
#' @param parameters List for the parameters used in the simulation. See
#' [default_parameters()] for a list of parameters used in the user-facing model
#' function [daedalus()].
#' @return A list with a single numeric vector of the same size as `state`,
#' suitable as output for \pkg{deSolve} functions such as [deSolve::lsoda()].
#' @details
#'
#' # Details: DAEDALUS model
#'
#' This section is a work in progress, and describes the DAEDALUS model in
#' brief.
#'
#' The model is age-stratified, supports heterogeneity in social contacts
#' between age groups, and currently supports four age groups.
#'
#' ## Epidemiological model
#'
#' The model has the following epidemiological compartments: susceptible
#' (\eqn{S}), exposed (\eqn{E}), infectious and symptomatic (\eqn{I_s}),
#' infectious asymptomatic (\eqn{I_a}), hospitalised (\eqn{H}), recovered
#' (\eqn{R}), and dead (\eqn{D}).
#'
#' The model currently allows only uniform parameters for transitions between
#' compartments. Group specific parameters are expected to be supported in
#' future.
#'
#' @keywords internal
.daedalus_ode <- function(t, state, parameters) {
  # prepare initial conditions
  n_age_groups <- 4L # hard-coded for four age groups

  # compartmental structure summary
  # nolint start
  # 1|2| 3| 4|5|6|7
  # S|E|Is|Ia|H|R|D
  # nolint end

  # hardcoding the array/tensor dimensions
  # 4 age groups, 7 epi compartments, single vaccination stratum (unvaccinated)
  # NOTE: DAEDALUS includes 3 vaccination strata and this implementation allows
  # for these to be added later
  state <- array(
    state,
    dim = c(n_age_groups, 7L, 1L)
  )

  # NOTE: all parameters are uniform across age groups
  beta <- parameters[["beta"]] # transmissibility parameter
  sigma <- parameters[["sigma"]] # exposed to infectious
  p_sigma <- parameters[["p_sigma"]] # proportion symptomatic
  epsilon <- parameters[["epsilon"]] # relative FOI from asymptomatics
  gamma <- parameters[["gamma"]] # single recovery rate for Ia, Is and Hosp.
  eta <- parameters[["eta"]] # hospitalisation rate for symptomatics
  omega <- parameters[["omega"]] # mortality rate for Hosp.

  # contact matrix
  cm <- parameters[["contact_matrix"]]

  # handle contribution of symptomatic and asymptomatic infections
  # NOTE: epsilon controls relative contribution of infectious asymptomatic
  new_infections <- beta * state[, 1L, ] *
    (cm %*% (state[, 3L, ] + state[, 4L, ] * epsilon))

  # create empty array of the dimensions of state
  d_state <- array(0.0, dim = dim(state))

  # change in susceptibles
  d_state[, 1L, ] <- -new_infections

  # change in exposed
  d_state[, 2L, ] <- new_infections - (sigma * state[, 2L, ])

  # change in infectious symptomatic
  d_state[, 3L, ] <- (p_sigma * sigma * state[, 2L, ]) -
    ((gamma + eta) * state[, 3L, ])

  # change in infectious asymptomatic
  d_state[, 4L, ] <- (sigma * (1.0 - p_sigma) * state[, 2L, ]) -
    (gamma * state[, 4L, ])

  # change in hospitalised
  d_state[, 5L, ] <- (eta * state[, 3L, ]) - ((gamma + omega) * state[, 5L, ])

  # change in recovered
  d_state[, 6L, ] <- (gamma * rowSums(state[, 3L:5L, ]))

  # change in dead
  d_state[, 7L, ] <- omega * state[, 5L, ]

  # return in the same order as state
  return(list(c(d_state)))
}
