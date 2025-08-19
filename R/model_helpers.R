#' Make large contact matrix for Cpp model.
#'
#' @name prepare_contacts
#' @rdname prepare_contacts
#'
#' @param country A `<daedalus_country>`.
#'
#' @keywords internal
#' @return
#'
#' 1. `make_conmat_large()` returns a 49x49 contact matrix scaled by the size of
#' demography groups.
#'
#' 2. `make_work_contacts()` returns a 45-element vector (for the number of
#' economic sectors) scaled by the number of workers per sector.
#'
#' 3. `make_consumer_contacts()` returns a 45x4 contact matrix with each row
#' scaled by the number of workers per sector. Dimensions are the number of
#' economic sectors and the number of age groups.
make_conmat_large <- function(country) {
  n_strata <- get_data(country, "n_strata")
  cm_nrow <- n_strata

  cm <- matrix(NA, cm_nrow, cm_nrow)
  cm[i_AGE_GROUPS, i_AGE_GROUPS] <- country$contact_matrix
  cm[i_AGE_GROUPS, i_ECON_SECTORS] <- matrix(
    cm[i_AGE_GROUPS, i_WORKING_AGE],
    N_AGE_GROUPS,
    N_ECON_SECTORS
  )
  cm[i_ECON_SECTORS, i_AGE_GROUPS] <- matrix(
    cm[i_WORKING_AGE, i_AGE_GROUPS],
    N_ECON_SECTORS,
    N_AGE_GROUPS,
    byrow = TRUE
  )

  cm[is.na(cm)] <- cm[i_WORKING_AGE, i_WORKING_AGE]

  demog <- rep(country$demography[i_WORKING_AGE], cm_nrow)
  demog[i_AGE_GROUPS] <- country$demography
  cm <- cm / demog

  cm
}

#' @name prepare_contacts
make_work_contacts <- function(country) {
  country$contacts_workplace / country$workers
}

#' @name prepare_contacts
make_consumer_contacts <- function(country) {
  # col-wise divison by demography
  country$contacts_consumer_worker %*% diag(1 / country$demography)
}

#' @title Generate a default initial state for DAEDALUS
#' @description Function to prepare the model initial state. Assumes that
#' 1 in every million individuals is initially infected, and that 60% are
#' asymptomatic infections. This does not affect the actual probability of
#' asymptomatic infections in the simulation, which is a property of a
#' `<daedalus_infection>`.
#'
#' @inheritParams daedalus
#'
#' @return An array with as many dimensions as `N_VACCINE_DATA_GROUPS`
#' (currently, 3); rows specify the age and economic groups, columns specify the
#' epidemiological compartments (including new infections and hospitalisations),
#' and array layers hold information on vaccination status (including new
#' vaccinations).
#' @keywords internal
make_initial_state <- function(country, initial_state_manual) {
  # NOTE: country checked in daedalus()
  n_strata <- get_data(country, "n_strata")
  initial_pop_state <- list(
    p_infectious = 1e-7,
    p_asymptomatic = 0.0,
    p_immune = rep(0.0, n_strata)
  )

  if (is.null(initial_state_manual)) {
    p_infectious <- initial_pop_state[["p_infectious"]]
    p_asymptomatic <- initial_pop_state[["p_asymptomatic"]]
    p_immune <- initial_pop_state[["p_immune"]]
  } else {
    initial_pop_state[names(initial_state_manual)] <- initial_state_manual

    p_infectious <- initial_pop_state[["p_infectious"]]
    p_asymptomatic <- initial_pop_state[["p_asymptomatic"]]
    p_immune <- initial_pop_state[["p_immune"]]

    # NOTE: no checks on country as this is tested in top-level fn `daedalus()`
    # check other inputs
    is_good_p_infectious <- checkmate::test_number(
      p_infectious,
      lower = 0.0,
      upper = 1.0
    )
    if (!is_good_p_infectious) {
      cli::cli_abort(
        "`p_infectious` must be a single number in the range [0.0, 1.0].",
        .envir = parent.frame()
      )
    }

    is_good_p_asymp <- checkmate::test_number(
      p_asymptomatic,
      lower = 0.0,
      upper = 1.0
    )
    if (!is_good_p_asymp) {
      cli::cli_abort(
        "`p_asymptomatic` must be a single number in the range [0.0, 1.0]."
      )
    }

    checkmate::assert_numeric(
      p_immune,
      0.0,
      1.0,
      finite = TRUE,
      any.missing = FALSE
    )
    checkmate::assert_subset(
      length(p_immune),
      c(1L, N_AGE_GROUPS, n_strata)
    )
    # handle immunity for workers
    if (length(p_immune) == N_AGE_GROUPS) {
      p_immune <- c(p_immune, rep(p_immune[i_WORKING_AGE], N_ECON_SECTORS))
    } else if (length(p_immune) == 1L) {
      p_immune <- rep(p_immune, n_strata)
    }
  }

  initial_state <- c(
    S = 1.0 - p_infectious,
    E = 0.0,
    Is = p_infectious * (1.0 - p_asymptomatic),
    Ia = p_infectious * p_asymptomatic,
    H = 0.0,
    R = 0.0,
    D = 0.0,
    dE = 0.0,
    dH = 0.0
  )

  # build for all age groups and economic sectors (working age only)
  initial_state <- matrix(
    initial_state,
    n_strata,
    N_MODEL_COMPARTMENTS,
    byrow = TRUE
  )

  # get demography and sector workforce, including non-working
  demography <- get_data(country, "demography")
  sector_workforce <- get_data(country, "workers")
  inactive_workers <- demography[i_WORKING_AGE] - sum(sector_workforce)
  demography[i_WORKING_AGE] <- inactive_workers

  # multiply by demography and sector workforce, row-wise
  initial_state[seq_len(N_AGE_GROUPS), ] <- initial_state[
    seq_len(N_AGE_GROUPS),
  ] *
    demography

  initial_state[(N_AGE_GROUPS + 1):nrow(initial_state), ] <- initial_state[
    (N_AGE_GROUPS + 1):nrow(initial_state),
  ] *
    sector_workforce

  # add strata for vaccination group
  initial_state <- array(
    initial_state,
    c(dim(initial_state), N_VACCINE_STRATA)
  )
  initial_state[,, i_VACCINATED_STRATUM] <-
    initial_state[,, i_UNVACCINATED_STRATUM] * p_immune
  initial_state[,, i_UNVACCINATED_STRATUM] <-
    initial_state[,, i_UNVACCINATED_STRATUM] * (1 - p_immune)

  # add state for new vaccinations by age group and econ sector
  state_new_vax <- numeric(
    length(get_data(country, "demography")) +
      length(get_data(country, "workers"))
  )
  initial_state <- c(
    initial_state,
    state_new_vax
  )

  initial_state
}

#' Get a vector of state indices
#'
#' @description
#' A helper function to get a vector of state indices for use in the
#' response sub-class constructors.
#'
#' @param state_name The state name as a string.
#'
#' @param country The country as a `<daedalus_country>`. Needed to
#' correctly calculate the number of age and economic sector groups.
#'
#' @return A vector of numbers representing indices.
#'
#' @keywords internal
get_state_indices <- function(state_name, country) {
  groups <- length(
    c(get_data(country, "demography"), get_data(country, "workers"))
  )

  # handle special cases
  if (state_name == "new_vax") {
    min_index <- N_MODEL_COMPARTMENTS * N_VACCINE_STRATA * groups
    max_index <- min_index + groups

    seq.int(min_index, max_index - 1)
  } else if (state_name == "ipr") {
    rel_flag_pos <- which(FLAG_NAMES == state_name)

    # calculate country state compartments
    total_compartments <- get_total_compartments(country)

    total_compartments + rel_flag_pos - 1L # passed to C++
  } else {
    compartment_index <- idx_COMPARTMENTS[[state_name]] # temporary
    max_index <- compartment_index * groups
    stride <- groups * N_MODEL_COMPARTMENTS
    max_indices <- max_index + stride * seq.int(0, N_VACCINE_STRATA - 1)

    as.vector(vapply(
      max_indices,
      function(x) {
        seq.int(x - groups, x - 1)
      },
      FUN.VALUE = integer(groups)
    ))
  }
}

#' @keywords internal
get_total_compartments <- function(country) {
  groups <- length(
    c(get_data(country, "demography"), get_data(country, "workers"))
  )

  # add groups at end for new vaccinations data
  total_compartments <- groups *
    N_MODEL_COMPARTMENTS *
    N_VACCINE_STRATA +
    groups

  total_compartments
}

#' @keywords internal
get_flag_index <- function(flag_name, country) {
  # get relative flag position
  rel_flag_pos <- which(FLAG_NAMES == flag_name)

  # calculate country state compartments
  total_compartments <- get_total_compartments(country)

  # return absolute flag pos
  total_compartments - 1L + rel_flag_pos
}
