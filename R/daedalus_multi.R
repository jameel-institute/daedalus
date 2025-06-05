#' Run daedalus for multiple infection parameter sets
#'
#' @description
#' Run [daedalus()] for multiple parameter sets, with the intended use case of
#' running the model under uncertainty in infection parameters.
#'
#' @inheritParams daedalus
#'
#' @param infection A list of `<daedalus_infection>` objects.
#'
#' @return A list of `<daedalus_output>` objects.
#'
#' @details
#'
#' See details of how `auto_social_distancing` affects the model in
#' documentation for [daedalus()].
#'
#' @export
daedalus_multi_infection <- function(
  country,
  infection,
  response_strategy = NULL,
  vaccine_investment = NULL,
  response_time = 30,
  response_duration = 365,
  auto_social_distancing = c("off", "independent", "npi_linked"),
  initial_state_manual = NULL,
  time_end = 600,
  ...
) {
  # prepare flags
  flags <- initial_flags()

  # input checking
  # NOTE: names are case sensitive
  country <- validate_country_input(country)

  # handle infection input and convert to a list of daedalus_infection
  infection <- validate_infection_list_input(infection)
  n_param_sets <- length(infection)

  # collect optional ODE control params and create ode_control obj
  ode_control <- rlang::list2(...)
  if (length(ode_control) > 0) {
    ode_control <- do.call(dust2::dust_ode_control, ode_control)
  } else {
    ode_control <- NULL
  }

  # checks on interventions
  # also prepare the appropriate economic openness vectors
  # allowing for a numeric vector, or NULL for truly no response
  if (is.null(response_strategy)) {
    response_strategy <- "none" # for output class
    openness <- rep(1.0, N_ECON_SECTORS)
    response_time <- NULL # to be filtered out later
  } else if (is.numeric(response_strategy)) {
    checkmate::assert_numeric(
      response_strategy,
      lower = 0,
      upper = 1,
      len = N_ECON_SECTORS
    )
    openness <- response_strategy
  } else if (
    length(response_strategy) == 1 &&
      response_strategy %in% names(daedalus.data::closure_data)
  ) {
    openness <- daedalus.data::closure_data[[response_strategy]]
  } else {
    cli::cli_abort(
      "Got an unexpected value for `response_strategy`. Options are `NULL`, \
    a numeric vector, or a recognised strategy. See function docs."
    )
  }

  # checks on vaccination
  vaccine_investment <- validate_vaccination_input(vaccine_investment)

  if (get_data(vaccine_investment, "start_time") == 0.0) {
    # check vaccination start time and set vaccination flag
    flags["vax_flag"] <- 1.0
  }

  is_good_time_end <- checkmate::test_count(time_end, positive = TRUE)
  if (!is_good_time_end) {
    cli::cli_abort(c(
      "Expected `time_end` to be a single positive integer-like number.",
      i = "E.g. `time_end = 100`, but not `time_end = 100.5`"
    ))
  }

  # NULL converted to "none"; WIP: this will be moved to a class constructor
  if (!identical(response_strategy, "none")) {
    is_good_response_time <- checkmate::test_integerish(
      response_time,
      upper = time_end, # for compat with daedalus
      lower = 1L, # responses cannot start at 0, unless strategy is null
      any.missing = FALSE,
      len = 1
    )
    if (!is_good_response_time) {
      cli::cli_abort(
        "Expected `response_time` to be between 1 and {time_end}."
      )
    }

    is_good_response_duration <- checkmate::test_count(
      response_duration
    )
    if (!is_good_response_duration) {
      cli::cli_abort(
        "Expected `response_duration` to be a single positive integer-like"
      )
    }
  } else {
    # set response time to NULL when response is NULL
    response_time <- NULL
  }

  #### spontaneous social distancing ####
  auto_social_distancing <- rlang::arg_match(auto_social_distancing)
  auto_social_distancing <- switch(
    auto_social_distancing,
    off = 0,
    independent = 1,
    npi_linked = 2
  )

  #### Prepare initial state and parameters ####
  initial_state <- make_initial_state(country, initial_state_manual)

  # prepare susceptibility matrix for vaccination
  susc <- make_susc_matrix(vaccine_investment, country)

  parameters <- lapply(infection, function(x) {
    c(
      prepare_parameters(country),
      prepare_parameters(x),
      prepare_parameters(vaccine_investment),
      list(
        beta = get_beta(x, country),
        susc = susc,
        openness = openness,
        response_time = response_time,
        response_duration = response_duration,
        auto_social_distancing = auto_social_distancing
      )
    )
  })

  # filter out NULLs so missing values can be read as NAN in C++
  parameters <- lapply(parameters, drop_null)

  output <- daedalus_internal(
    time_end,
    parameters,
    initial_state,
    flags,
    ode_control,
    n_param_sets
  )
  output_data <- prepare_output(output$data, country)

  # NOTE: needs to be compatible with `<daedalus_output>`
  # or equivalent from `{daedalus.compare}`
  stopifnot(
    "Length of outputs and infections is not the same" = length(infection) ==
      length(output_data)
  )

  closure_info <- lapply(
    output$event_data,
    get_daedalus_response_times,
    time_end
  )

  # return list of daedalus_output
  Map(
    output_data,
    infection,
    closure_info,
    f = function(x, y, z) {
      o <- list(
        total_time = time_end,
        model_data = x,
        country_parameters = unclass(country),
        infection_parameters = unclass(y),
        response_data = list(
          response_strategy = response_strategy,
          openness = openness,
          closure_info = z
        )
      )

      as_daedalus_output(o)
    }
  )
}
