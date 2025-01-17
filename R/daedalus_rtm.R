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
  cm_nrow <- N_AGE_GROUPS + N_ECON_SECTORS

  cm <- matrix(NA, cm_nrow, cm_nrow)
  cm[i_AGE_GROUPS, i_AGE_GROUPS] <- country$contact_matrix
  cm[i_AGE_GROUPS, i_ECON_SECTORS] <- matrix(
    cm[i_AGE_GROUPS, i_WORKING_AGE], N_AGE_GROUPS, N_ECON_SECTORS
  )
  cm[i_ECON_SECTORS, i_AGE_GROUPS] <- matrix(
    cm[i_WORKING_AGE, i_AGE_GROUPS], N_ECON_SECTORS, N_AGE_GROUPS,
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
  # row-wise divison
  country$contacts_consumer_worker / country$workers
}

#' @title Model epidemic and economic outcomes under bespoke interventions
#'
#' @description
#' Allows modelling of epidemic trajectories and economic costs similar to
#' [daedalus()], but with the start and duration of restrictions controlled by
#' the user. No reactive interventions are allowed, and vaccination is not
#' implemented. Offers the option to model spontaneous social distancing, and
#' allows exogeneous reduction of the transmission rate to simulate the effect
#' of measures that reduce transmission (such as social distancing or mask
#' mandates).
#'
#' Also allows basic modelling of parameter uncertainty by passing a list of
#' infection parameter combinations, see `infection` below.
#'
#' @inheritParams daedalus
#'
#' @param infection Similar to [daedalus()], may be a character vector from
#' among [daedalus::epidemic_names] or a `<daedalus_infection>` object.
#' May also be a list of `<daedalus_infection>` objects.
#'
#' @param response_strategy Either a string for the name of response strategy
#' followed from among "none", "school_closures", "economic_closures", and
#' "elimination", or a numeric vector of the same length as the number of
#' economic sectors (45) giving the openness coefficient for each sector when
#' closures are active.
#'
#' @param response_time_start A single number for the time at which the
#' `response_strategy` comes into effect.
#'
#' @param response_time_end A single number of the time at which the response
#' comes to an end.
#'
#' @param hospital_capacity A single number specifying the hospital capacity
#' dedicated to pandemic response. When hospital demand crosses this value,
#' the `infection` parameter `omega` is increased by a factor of 1.6.
#'
#' @param initial_state_manual An optional **named** list with the names
#' `p_infectious` and `p_asymptomatic` for the proportion of infectious and
#' symptomatic individuals in each age group and economic sector.
#' Defaults to `1e-7` and `0.0` respectively.
#'
#' @param auto_social_distancing A single logical value for whether spontaneous
#' social distancing is active. When active, social distancing is dependent on
#' daily deaths and is active even when no response is active. Defaults to
#' `FALSE`.
#'
#' @param social_distancing_mandate A single number for the scaling of the
#' transmission rate due to other measures taken to reduce transmission, such as
#' mask mandates. This scaling is only active between `response_time_start` and
#' `response_time_end`, and is active even when no response strategy is
#' specified. Defaults to 1.0.
#'
#' @param time_end The end point of the simulation, defaults to 300 days.
#'
#' @export
daedalus_rtm <- function(country,
                         infection,
                         response_strategy = NULL,
                         response_time_start = 0.0,
                         response_time_end = 0.0,
                         hospital_capacity = NULL,
                         initial_state_manual = list(p_infectious = 1e-7),
                         auto_social_distancing = FALSE,
                         social_distancing_mandate = 1.0,
                         time_end = 300) {
  # input checking
  # NOTE: names are case sensitive
  checkmate::assert_multi_class(country, c("daedalus_country", "character"))
  if (is.character(country)) {
    country <- daedalus_country(country)
  }
  checkmate::assert_multi_class(
    infection, c("daedalus_infection", "character", "list")
  )
  if (is.character(infection)) {
    infection <- rlang::arg_match(infection, daedalus::epidemic_names)
    infection <- daedalus_infection(infection)
    parameters <- prepare_parameters.daedalus_infection(infection)
    parameters$beta <- get_beta(infection, country)
    parameters <- list(parameters)
  } else if (is_daedalus_infection(infection)) {
    parameters <- prepare_parameters.daedalus_infection(infection)
    parameters$beta <- get_beta(infection, country)
    parameters <- list(parameters)
  } else if (checkmate::test_list(infection, types = "daedalus_infection")) {
    z <- prepare_parameters.daedalus_infection(infection[[1]]) # nolint

    parameters <- lapply(
      infection, function(x) {
        z$beta <- get_beta(x, country)
        z
      }
    )
  } else {
    cli::cli_abort(
      "`infection` must be a character from among `daedalus::epidemic_names`,
      a `<daedalus_infection>`, or a list of `<daedalus_infection>`s."
    )
  }

  is_good_time_end <- checkmate::test_count(time_end, positive = TRUE)
  if (!is_good_time_end) {
    cli::cli_abort(
      c(
        "Expected `time_end` to be a single positive integer-like number.",
        i = "E.g. `time_end = 100`, but not `time_end = 100.5`"
      )
    )
  }

  # prepare initial state for Cpp model
  initial_state <- make_initial_state(country, initial_state_manual)
  initial_state <- as.matrix(initial_state[, , 1])
  initial_state <- cbind(initial_state, matrix(0, nrow(initial_state), 1))

  # prepare the appropriate economic openness vectors
  # allowing for a numeric vector, or NULL for no response
  if (is.null(response_strategy)) {
    openness <- rep(1.0, N_ECON_SECTORS)
    resp_strat_tag <- "none"
  } else if (is.numeric(response_strategy)) {
    openness <- response_strategy
    resp_strat_tag <- "manual"
  } else if (response_strategy %in% names(daedalus::closure_data)) {
    openness <- daedalus::closure_data[[response_strategy]]
    resp_strat_tag <- response_strategy
  }

  # prepare within workplace and community contacts
  contact_matrix <- make_conmat_large(country)
  contacts_work <- make_work_contacts(country)
  contacts_consumers <- make_consumer_contacts(country)

  # prepare hospital capacity from country data if none specified
  if (is.null(hospital_capacity)) {
    hospital_capacity <- get_data(country, "hospital_capacity")
  }

  # ode solving
  output <- .model_daedalus_cpp(
    initial_state = initial_state,
    params = parameters,
    contact_matrix = contact_matrix,
    contacts_work = contacts_work,
    contacts_consumers = contacts_consumers,
    openness = openness,
    hospital_capacity = hospital_capacity,
    t_start = response_time_start,
    t_end = response_time_end,
    auto_social_distancing,
    social_distancing_mandate = social_distancing_mandate,
    time_end
  )

  output <- Map(output, seq_along(output), f = function(x, y) {
    z <- prepare_output_cpp(x)
    z$replicate <- y

    z
  })

  # NOTE: need to handle case when response is triggered after `time_end`
  # this is needed when running sims for different durations to examine
  # how costs evolve over time
  if (time_end < response_time_start) {
    response_time_start <- 0
    response_time_end <- 0
  }
  if (time_end < response_time_end) {
    response_time_end <- time_end
  }

  output <- lapply(
    output, function(x) {
      z <- list(
        total_time = time_end,
        model_data = x,
        country_parameters = unclass(country),
        infection_parameters = unclass(infection),
        response_data = list(
          response_strategy = resp_strat_tag,
          openness = openness,
          closure_info = list(
            closure_time_start = response_time_start,
            closure_time_end = response_time_end,
            closure_duration = response_time_end - response_time_start
          )
        )
      )

      as_daedalus_output(z)
    }
  )

  if (length(output) == 1) {
    output[[1]]
  } else {
    output
  }
}
