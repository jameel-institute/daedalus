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
#' [daedalus()] for a list of model parameters.
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
  state_ <- state[-length(state)]
  state_ <- array(
    state_,
    c(N_AGE_GROUPS, N_EPI_COMPARTMENTS, N_ECON_STRATA)
  )

  r0 <- parameters[["r0"]]
  sigma <- parameters[["sigma"]] # exposed to infectious
  p_sigma <- parameters[["p_sigma"]] # proportion symptomatic
  epsilon <- parameters[["epsilon"]] # relative FOI from asymptomatics
  gamma_Is <- parameters[["gamma_Is"]] # single recovery rate for Is
  gamma_Ia <- parameters[["gamma_Ia"]] # recovery rate for Ia
  rho <- parameters[["rho"]] # waning rate for infection-derived immunity

  # NOTE: these params are vectors of length `N_AGE_GROUPS`
  gamma_H <- parameters[["gamma_H"]] # recovery rate for H
  eta <- parameters[["eta"]] # hospitalisation rate for symptomatics
  omega <- parameters[["omega"]] # mortality rate for Hosp.

  cm <- parameters[["contact_matrix"]]
  cmw <- parameters[["contacts_workplace"]]
  # NOTE: consumer-worker contacts are known per sector, and are scaled during
  # pre-processing by the demographic distribution for an assumption of
  # proportionality with population demography. This may need to change if
  # epi compartments preventing contacts - hospitalisation and death - have
  # age-specific entry rates (e.g. older people are hospitalised more).
  cw <- parameters[["contacts_consumer_worker"]]
  # NOTE: worker contacts between sectors takes a dummy value of 1e-6
  cm_ww <- parameters[["contacts_between_sectors"]]

  # NOTE: `demography` includes the hospitalised and dead. Should probably be
  # removed. May not be a major factor as mortality rate * hosp_rate is low.
  demography <- parameters[["demography"]]

  # scaling economic sector openness
  openness <- parameters[["openness"]]
  switch <- state["switch"]
  scaling <- 1 - (1 - openness) * switch # clunky
  r0_econ <- r0 * scaling
  r0 <- r0 * mean(scaling) # as otherwise no scaling on r0

  # NOTE: epsilon controls relative contribution of infectious asymptomatic
  new_community_infections <- r0 * state_[, i_S, ] *
    cm %*% (state_[, i_Is, ] + state_[, i_Ia, ] * epsilon)

  workplace_infected <- state_[i_WORKING_AGE, i_Is, -i_NOT_WORKING] +
    state_[i_WORKING_AGE, i_Ia, -i_NOT_WORKING] * epsilon

  # NOTE: original DAEDALUS model lumped economic sectors with age strata, here
  # we use a 3D tensor instead, where the first layer represents the non-working
  new_workplace_infections <- r0_econ *
    state_[i_WORKING_AGE, i_S, -i_NOT_WORKING] *
    ((cmw * workplace_infected) + c(workplace_infected %*% cm_ww)) /
    colSums(state_[i_WORKING_AGE, , -i_NOT_WORKING])

  # NOTE: only consumer to worker infections are currently allowed
  # NOTE: calculate FOI as β * Σ_{j=1}^{j=N} M_{ij} I_j / N_j
  infected_consumers <- (state_[, i_Is, i_NOT_WORKING] +
    state_[, i_Ia, i_NOT_WORKING] * epsilon) / demography
  foi_cw <- r0_econ * cw %*% infected_consumers

  new_comm_work_infections <- state_[i_WORKING_AGE, i_S, -i_NOT_WORKING] *
    foi_cw

  # create empty array of the dimensions of state
  d_state <- array(0.0, dim(state_))

  # change in susceptibles
  d_state[, i_S, ] <- -new_community_infections + (rho * state_[, i_R, ])
  d_state[i_WORKING_AGE, i_S, -i_NOT_WORKING] <-
    d_state[i_WORKING_AGE, i_S, -i_NOT_WORKING] -
    new_workplace_infections - new_comm_work_infections

  # change in exposed
  d_state[, i_E, ] <- new_community_infections - sigma * state_[, i_E, ]
  d_state[i_WORKING_AGE, i_E, -i_NOT_WORKING] <-
    d_state[i_WORKING_AGE, i_E, -i_NOT_WORKING] +
    new_workplace_infections + new_comm_work_infections

  # change in infectious symptomatic
  d_state[, i_Is, ] <- p_sigma * sigma * state_[, i_E, ] -
    (gamma_Is + eta) * state_[, i_Is, ]

  # change in infectious asymptomatic
  d_state[, i_Ia, ] <- sigma * (1.0 - p_sigma) * state_[, i_E, ] -
    gamma_Ia * state_[, i_Ia, ]

  # change in hospitalised
  d_state[, i_H, ] <- eta * state_[, i_Is, ] -
    (gamma_H + omega) * state_[, i_H, ]

  # change in recovered - NOTE: different recovery rates for each compartment
  d_state[, i_R, ] <- gamma_Is * state_[, i_Is, ] +
    gamma_Ia * state_[, i_Ia, ] +
    gamma_H * state_[, i_H, ] -
    rho * state_[, i_R, ]

  # change in dead
  d_state[, i_D, ] <- omega * state_[, i_H, ]

  d_switch <- 0.0

  # return in the same order as state
  list(c(d_state, d_switch))
}
