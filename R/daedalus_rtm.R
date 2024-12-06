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
                         initial_state_manual = list(p_infectious = 1e-7),
                         auto_social_distancing = FALSE,
                         time_end = 300) {
  # # input checking
  # # NOTE: names are case sensitive
  # checkmate::assert_multi_class(country, c("daedalus_country", "character"))
  if (is.character(country)) {
    country <- daedalus_country(country)
  }
  # checkmate::assert_multi_class(infection, c("daedalus_infection", "character"))
  if (is.character(infection)) {
    # infection <- rlang::arg_match(infection, daedalus::epidemic_names)
    infection <- daedalus_infection(infection)
    parameters <- prepare_parameters.daedalus_infection(infection)
    parameters$eta <- parameters$eta[1]
    parameters$omega <- parameters$omega[1]
  } else if (checkmate::test_list(infection, types = "daedalus_infection")) {
    z <- prepare_parameters.daedalus_infection(infection[[1]])
    z$eta <- z$eta[1]
    z$omega <- z$omega[1]

    parameters <- lapply(
      infection, function(x) {
        z$beta <- get_beta(x, country)
        z
      }
    )
  }

  # is_good_time_end <- checkmate::test_count(time_end, positive = TRUE)
  # if (!is_good_time_end) {
  #   cli::cli_abort(
  #     c(
  #       "Expected `time_end` to be a single positive integer-like number.",
  #       i = "E.g. `time_end = 100`, but not `time_end = 100.5`"
  #     )
  #   )
  # }

  # is_good_response_time <- checkmate::test_integerish(
  #   response_time_start,
  #   upper = time_end - 2L, lower = 2L, any.missing = FALSE
  # )
  # if (!is_good_response_time) {
  #   cli::cli_abort(
  #     "Expected `response_time` to be between 2 and {time_end - 2L}."
  #   )
  # }

  # prepare initial state for Cpp model
  initial_state <- make_initial_state(country, initial_state_manual)
  initial_state <- as.matrix(initial_state[, , 1])
  initial_state <- cbind(initial_state, matrix(0, nrow(initial_state), 1))

  # parameters <- c(
  #   # prepare_parameters(country),
  #   prepare_parameters.daedalus_infection(infection)
  # )

  # hotfix
  # parameters$eta <- parameters$eta[1]
  # parameters$omega <- parameters$omega[1]

  # add the appropriate economic openness vectors to parameters
  if (is.null(response_strategy)) {
    openness <- rep(1.0, N_ECON_SECTORS)
  } else if (is.numeric(response_strategy)) {
    openness <- response_strategy
  } else if (response_strategy %in% names(closure_data)) {
    openness <- daedalus::closure_data[[response_strategy]]
  }

  # NOTE: psi (vax waning rate), tau (vax reduction in suscept.), and dims of nu
  # are hard-coded until vaccination scenarios are decided
  # parameters <- c(
  #   parameters,
  #   list(
  #     # to increase HFR if crossed
  #     hospital_capacity = get_data(country, "hospital_capacity"),
  #     # psi = 1 / 270,
  #     # tau = c(1.0, 0.5),
  #     # beta = get_beta(infection, country),
  #     openness = openness
  #   )
  # )

  contact_matrix <- make_conmat_large(country)
  contacts_work <- make_work_contacts(country)

  # ode solving
  # output <- lapply(
  #   parameters,
  #   function(x) {
      output = .model_daedalus_cpp(
        initial_state = initial_state,
        params = parameters,
        contact_matrix = contact_matrix,
        contacts_work = contacts_work,
        openness = openness,
        t_start = response_time_start,
        t_end = response_time_end,
        auto_social_distancing,
        time_end
      )
  #   }
  # )

  output <- Map(output, seq_along(output), f = function(x, y) {
    z <- prepare_output_cpp(x)
    z$replicate <- y
  
    z
  })
  
  data = data.table::rbindlist(output)
  
  data.table::setDF(data)
}
