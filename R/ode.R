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
  state_ <- values_to_state(state)

  # remove delta vaccinations layer
  state_ <- state_[, , -i_NEW_VAX_STRATUM]

  #### Parameter preparation ####
  beta <- parameters[["beta"]]
  sigma <- parameters[["sigma"]] # exposed to infectious
  p_sigma <- parameters[["p_sigma"]] # proportion symptomatic
  epsilon <- parameters[["epsilon"]] # relative FOI from asymptomatics
  gamma_Is <- parameters[["gamma_Is"]] # single recovery rate for Is
  gamma_Ia <- parameters[["gamma_Ia"]] # recovery rate for Ia
  rho <- parameters[["rho"]] # waning rate for infection-derived immunity
  nu <- parameters[["nu"]] # vaccination rate as a proportion of the total pop
  psi <- parameters[["psi"]] # waning rate of vaccination derived immunity
  tau <- parameters[["tau"]] # susceptibility of vaccination strata; length 2L
  vax_uptake_limit <- parameters[["vax_uptake_limit"]]

  # NOTE: these params are vectors of length `N_AGE_GROUPS`
  gamma_H <- parameters[["gamma_H"]] # recovery rate for H
  eta <- parameters[["eta"]] # hospitalisation rate for symptomatics

  # increase mortality rate to 1.6x if hospitals are over capacity
  hosp_switch <- rlang::env_get(parameters[["mutables"]], "hosp_switch")
  omega <- parameters[["omega"]] * ifelse(hosp_switch, 1.6, 1.0)

  cm <- parameters[["contact_matrix"]]
  cmw <- parameters[["contacts_workplace"]]
  # NOTE: consumer-worker contacts are known per sector, and are scaled during
  # pre-processing by the demographic distribution for an assumption of
  # proportionality with population demography. This may need to change if
  # epi compartments preventing contacts - hospitalisation and death - have
  # age-specific entry rates (e.g. older people are hospitalised more).
  cw <- parameters[["contacts_consumer_worker"]]

  # NOTE: `demography` includes the hospitalised and dead. Should probably be
  # removed. May not be a major factor as mortality rate * hosp_rate is low.
  demography <- parameters[["demography"]]

  # scaling economic sector openness
  openness <- parameters[["openness"]]

  switch <- rlang::env_get(parameters[["mutables"]], "switch")

  scaling <- if (switch) openness else 1.0
  beta_econ <- beta * scaling

  # NOTE: scale vax rate by proportion of eligible individuals remaining
  # to maintain rate relative to total population as eligibles decrease
  vax_switch <- rlang::env_get(parameters[["mutables"]], "vax_switch")
  nu <- if (vax_switch) scale_nu(state_, nu, vax_uptake_limit) else 0.0

  # create empty array of the dimensions of `state_`
  d_state <- array(0.0, dim(state_))

  #### Social distancing ####
  # get new deaths and implement social distancing only when closures are active
  # as described in https://github.com/robj411/p2_drivers
  new_deaths <- state_[, i_H, ] * omega # row i by element i
  d_state[, i_D, ] <- new_deaths
  new_deaths_total <- sum(new_deaths)

  beta <- beta * if (switch) {
    get_distancing_coefficient(new_deaths_total)
  } else {
    1.0
  }

  #### Force of infection calculations ####
  # NOTE: get total number in each age group infectious
  # NOTE: epsilon controls relative contribution of infectious asymptomatic
  infectious <- state_[, i_Is, ] + state_[, i_Ia, ] * epsilon
  community_infectious <- rowSums(infectious)

  community_infectious[i_WORKING_AGE] <- community_infectious[i_WORKING_AGE] +
    sum(community_infectious[i_ECON_SECTORS])

  community_infectious <- community_infectious[i_AGE_GROUPS]

  cm_inf <- beta * cm %*% (community_infectious)

  # NOTE: `state` and `cm_inf` mult. assumes length(cm_inf) == nrows(state)
  new_community_infections <- state_[, i_S, ]
  new_community_infections[i_AGE_GROUPS, ] <- state_[i_AGE_GROUPS, i_S, ] *
    as.vector(cm_inf)
  new_community_infections[i_ECON_SECTORS, ] <- state_[i_ECON_SECTORS, i_S, ] *
    cm_inf[i_WORKING_AGE]
  # scaling by vaccine-reduced susceptibility
  # NOTE: add MISSING scaling by beta here for βSI/N
  new_community_infections <- new_community_infections %*% diag(tau)

  # NOTE: re-assigning `workplace_infected`
  workplace_infected <- infectious[i_ECON_SECTORS, ]
  workplace_infected <- cmw * workplace_infected %*% diag(tau)
  workplace_infected <- rowSums(workplace_infected)

  # workplace infections from other workers w/ reduced susceptibility of vaxxed
  # NOTE: explicit col-wise multiplication of workplace infected * tau
  new_workplace_infections <- beta_econ *
    state_[i_ECON_SECTORS, i_S, ] * workplace_infected

  # NOTE: only consumer to worker infections are currently allowed
  # NOTE: only infections from non-working consumers currently possible
  infected_consumers <- (state_[i_AGE_GROUPS, i_Is, ] +
    state_[i_AGE_GROUPS, i_Ia, ] * epsilon) / demography
  foi_cw <- beta_econ * cw %*% infected_consumers

  # force col-wise multiplication of vax-derived reduction in susceptibility
  new_comm_work_infections <- state_[i_ECON_SECTORS, i_S, ] %*%
    diag(tau) * foi_cw

  #### State change equations ####
  # change in susceptibles
  d_state[, i_S, ] <- -new_community_infections + (rho * state_[, i_R, ])
  d_state[i_ECON_SECTORS, i_S, ] <-
    d_state[i_ECON_SECTORS, i_S, ] -
    new_workplace_infections - new_comm_work_infections

  # log new infections
  d_state[, i_dE, ] <- new_community_infections
  d_state[i_ECON_SECTORS, i_dE, ] <-
    d_state[i_ECON_SECTORS, i_dE, ] +
    new_workplace_infections + new_comm_work_infections

  # change in exposed
  d_state[, i_E, ] <- d_state[, i_dE, ] - (sigma * state_[, i_E, ])

  # log new hospitalisations
  d_state[, i_dH, ] <- state_[, i_Is, ] * eta

  # change in infectious symptomatic
  d_state[, i_Is, ] <- p_sigma * sigma * state_[, i_E, ] -
    (gamma_Is * state_[, i_Is, ]) - d_state[, i_dH, ]

  # change in infectious asymptomatic
  d_state[, i_Ia, ] <- sigma * (1.0 - p_sigma) * state_[, i_E, ] -
    gamma_Ia * state_[, i_Ia, ]

  # change in hospitalised
  d_state[, i_H, ] <- d_state[, i_dH, ] - d_state[, i_D, ] -
    gamma_H * state_[, i_H, ]

  # change in recovered - NOTE: different recovery rates for each compartment
  d_state[, i_R, ] <- gamma_Is * state_[, i_Is, ] +
    gamma_Ia * state_[, i_Ia, ] +
    gamma_H * state_[, i_H, ] -
    rho * state_[, i_R, ]

  #### Vaccination and vaccine waning ####
  # add empty array for new vax
  d_vax_temp <- matrix(
    0, N_AGE_GROUPS + N_ECON_SECTORS, N_MODEL_COMPARTMENTS
  )
  d_state <- values_to_state(
    c(d_state, d_vax_temp)
  )

  # log new vaccinations
  d_state[, c(i_S, i_R), i_NEW_VAX_STRATUM] <-
    state_[, c(i_S, i_R), i_UNVACCINATED_STRATUM] * nu

  # NOTE: changes in S and R are on top of previous changes due to infection,
  # recovery, and waning
  # change in vaccinated: only susceptible and recovered are vaccinated
  d_state[, c(i_S, i_R), i_VACCINATED_STRATUM] <-
    d_state[, c(i_S, i_R), i_VACCINATED_STRATUM] +
    state_[, c(i_S, i_R), i_UNVACCINATED_STRATUM] * nu -
    state_[, c(i_S, i_R), i_VACCINATED_STRATUM] * psi

  # change in unvaccinated: assume waning only for susceptible and recovered
  d_state[, c(i_S, i_R), i_UNVACCINATED_STRATUM] <-
    d_state[, c(i_S, i_R), i_UNVACCINATED_STRATUM] +
    state_[, c(i_S, i_R), i_VACCINATED_STRATUM] * psi -
    state_[, c(i_S, i_R), i_UNVACCINATED_STRATUM] * nu

  # return in the same order as state
  list(c(d_state))
}
