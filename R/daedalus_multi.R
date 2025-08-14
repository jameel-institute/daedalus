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

  # for more informative errors as fn uses `parent.frame()`
  npi <- validate_npi_input(
    response_strategy,
    country,
    first(infection),
    response_time,
    response_duration
  )

  # NPI needs gamma_Ia from each infection
  npi <- lapply(infection, function(x) {
    validate_npi_input(
      response_strategy,
      country,
      x,
      response_time,
      response_duration
    )
  })

  # get only first NPI identifier
  response_identifier <- first(npi)$parameters$identifier

  # checks on vaccination input; make copy to allow for true vax start at 0.0
  # if users want that
  vaccination <- validate_vaccination_input(vaccine_investment, country)

  if (get_data(vaccination, "start_time") == 0.0) {
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

  # prevent passing NAs as these are not correctly handled on C++ side
  if (identical(response_strategy, "none") || is.null(response_strategy)) {
    # set response time to NULL when response is NULL
    response_time <- NULL
    duration <- NULL
  } else {
    response_time <- npi$time_on
    duration <- npi$duration
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
  susc <- make_susc_matrix(vaccination, country)

  # as both infection and npi are now lists
  parameters <- Map(infection, npi, f = function(x, y) {
    c(
      prepare_parameters(country),
      prepare_parameters(x),
      prepare_parameters(vaccination),
      list(
        beta = get_beta(x, country),
        susc = susc,
        openness = get_data(first(npi), "openness"),
        response_time = response_time,
        response_duration = duration,
        auto_social_distancing = auto_social_distancing,
        vaccination = vaccination,
        npi = y
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
    output$event_data,
    f = function(x, y, z, zz) {
      o <- list(
        total_time = time_end,
        model_data = x,
        country_parameters = unclass(country),
        infection_parameters = unclass(y),
        response_data = list(
          response_strategy = response_identifier,
          openness = get_data(npi[[1]], "openness"), # from first NPI
          closure_info = z
        ),
        event_data = zz
      )

      as_daedalus_output(o)
    }
  )
}
