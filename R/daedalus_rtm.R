#' Make large contact matrix for Cpp model.
#'
#' @param country A `<daedalus_country>`.
#' @return A 49x49 contact matrix.
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

make_work_contacts <- function(country) {
  country$contacts_workplace / country$workers
}

#' @title DAEDALUS Cpp model
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
    z <- prepare_parameters.daedalus_infection(infection[[1]])

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
