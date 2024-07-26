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
daedalus_rhs <- function(t, state, parameters) {
  # NOTE: see constants.R for compartmental indices
  # NOTE: DAEDALUS includes 45 vaccination strata for economic sectors,
  # and these are represented by the third dimension of the tensor
  state <- array(
    state,
    dim = c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_STRATA)
  )

  # NOTE: all rate parameters are uniform across age groups, this may change
  # in future.
  beta <- parameters[["beta"]] # transmissibility parameter
  sigma <- parameters[["sigma"]] # exposed to infectious
  p_sigma <- parameters[["p_sigma"]] # proportion symptomatic
  epsilon <- parameters[["epsilon"]] # relative FOI from asymptomatics
  gamma <- parameters[["gamma"]] # single recovery rate for Ia, Is and Hosp.
  eta <- parameters[["eta"]] # hospitalisation rate for symptomatics
  omega <- parameters[["omega"]] # mortality rate for Hosp.
  rho <- parameters[["rho"]] # waning rate for infection-derived immunity
  cm <- parameters[["contact_matrix"]]

  # mean contact rate in each sector; contacts ÷ sector population
  cmw <- parameters[["contacts_workplace"]]

  # NOTE: consumer-worker contacts are known per sector, and are scaled during
  # pre-processing by the demographic distribution for an assumption of
  # proportionality with population demography. This may need to change if
  # epi compartments preventing contacts - hospitalisation and death - have
  # age-specific entry rates (e.g. older people are hospitalised more).
  cw <- parameters[["contacts_consumer_worker"]]

  # NOTE: `demography` includes the hospitalised and dead. Should probably be
  # removed. May not be a major factor as mortality rate * hosp_rate is low.
  demography <- rowSums(state)

  # NOTE: epsilon controls relative contribution of infectious asymptomatic
  new_community_infections <- beta * state[, i_S, ] *
    (cm %*% (state[, i_Is, ] + state[, i_Ia, ] * epsilon))

  workplace_infected <- state[i_WORKING_AGE, i_Is, -i_NOT_WORKING] +
    state[i_WORKING_AGE, i_Ia, -i_NOT_WORKING] * epsilon

  # NOTE: original DAEDALUS model lumped economic sectors with age strata, here
  # we use a 3D tensor instead, where the first layer represents the non-working
  new_workplace_infections <- beta *
    state[i_WORKING_AGE, i_S, -i_NOT_WORKING] *
    cmw * workplace_infected /
    colSums(state[i_WORKING_AGE, , -i_NOT_WORKING])

  # NOTE: only consumer to worker infections are currently allowed
  new_comm_work_infections <- beta *
    state[i_WORKING_AGE, i_S, -i_NOT_WORKING] *
    (
      cw %*% (
        (
          state[, i_Is, i_NOT_WORKING] + state[, i_Ia, i_NOT_WORKING] * epsilon
        ) / demography
      )
    )

  # create empty array of the dimensions of state
  d_state <- array(0.0, dim = dim(state))

  # change in susceptibles
  d_state[, i_S, ] <- -new_community_infections + (rho * state[, i_R, ])
  d_state[i_WORKING_AGE, i_S, -i_NOT_WORKING] <-
    d_state[i_WORKING_AGE, i_S, -i_NOT_WORKING] -
    new_workplace_infections - new_comm_work_infections

  # change in exposed
  d_state[, i_E, ] <- new_community_infections - (sigma * state[, i_E, ])
  d_state[i_WORKING_AGE, i_E, -i_NOT_WORKING] <-
    d_state[i_WORKING_AGE, i_E, -i_NOT_WORKING] +
    new_workplace_infections + new_comm_work_infections

  # change in infectious symptomatic
  d_state[, i_Is, ] <- (p_sigma * sigma * state[, i_E, ]) -
    ((gamma + eta) * state[, i_Is, ])

  # change in infectious asymptomatic
  d_state[, i_Ia, ] <- (sigma * (1.0 - p_sigma) * state[, i_E, ]) -
    (gamma * state[, i_Ia, ])

  # change in hospitalised
  d_state[, i_H, ] <- (eta * state[, i_Is, ]) -
    ((gamma + omega) * state[, i_H, ])

  # change in recovered
  # NOTE: rowSums does not accept a `dims` argument, hence `apply()`
  d_state[, i_R, ] <- (gamma *
    apply(
      X = state[, c(i_Is, i_Ia, i_H), ],
      MARGIN = c(DIM_AGE_GROUPS, DIM_ECON_SECTORS),
      FUN = sum
    )
  ) -
    (rho * state[, i_R, ])

  # change in dead
  d_state[, i_D, ] <- omega * state[, i_H, ]

  # return in the same order as state
  list(c(d_state))
}
