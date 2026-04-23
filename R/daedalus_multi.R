#' Run daedalus for multiple infection parameter sets
#'
#' @description
#' Run [daedalus()] for multiple parameter sets, with the intended use case of
#' running the model under uncertainty in infection parameters.
#'
#' @inheritParams daedalus
#'
#' @param infection A list of `<daedalus_infection>` objects. Must have a
#' minimum length of 2.
#'
#' @return A list of `<daedalus_output>` objects of the same length as
#' `infection`.
#'
#' @export
daedalus_multi_infection <- function(
  country,
  infection,
  response_strategy = NULL,
  vaccine_investment = NULL,
  behaviour = NULL,
  response_time = 30,
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

  flags_list <- lapply(infection, function(l) {
    flags["ipr"] <- l$r0
    flags
  })

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
    response_time
  )

  # NPI needs gamma_Ia from each infection
  npi <- lapply(infection, function(x) {
    validate_npi_input(
      response_strategy,
      country,
      x,
      response_time
    )
  })

  # get only first NPI identifier
  response_identifier <- first(npi)$identifier

  # checks on vaccination input; make copy to allow for true vax start at 0.0
  # if users want that
  vaccination <- validate_vaccination_input(vaccine_investment, country)

  if (
    get_data(vaccination, "start_time") == 0.0 &&
      (!is.null(vaccine_investment))
  ) {
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

  #### BEHAVIOURAL MODULE ####
  # validate input and set flag to on if not null
  behaviour <- validate_behaviour_input(behaviour)
  if (behaviour$identifier != "no_behaviour") {
    flags["behav_flag"] <- 1.0
  }

  #### Prepare initial state and parameters ####
  initial_state <- make_initial_state(country, initial_state_manual)

  # prepare susceptibility matrix for vaccination
  susc <- make_susc_matrix(vaccination, country)

  hosp_overflow <- new_daedalus_hosp_overflow(country)

  # as both infection and npi are now lists
  parameters <- Map(infection, npi, f = function(x, y) {
    c(
      prepare_parameters(country),
      prepare_parameters(x),
      prepare_parameters(vaccination),
      prepare_parameters(behaviour),
      list(
        beta = get_beta(x, country),
        susc = susc,
        ngm = get_ngm(country, x),
        vaccination = vaccination,
        npi = y,
        hosp_overflow = hosp_overflow
      )
    )
  })

  # filter out NULLs so missing values can be read as NAN in C++
  parameters <- lapply(parameters, drop_null)

  output <- daedalus_internal(
    time_end,
    parameters,
    initial_state,
    flags_list,
    ode_control,
    n_param_sets
  )

  output_data_list <- split_multi_soln(output)

  # NOTE: needs to be compatible with `<daedalus_output>`
  # or equivalent from `{daedalus.compare}`
  stopifnot(
    "Length of outputs and infections is not the same" = n_param_sets ==
      length(output_data_list)
  )

  event_info <- get_daedalus_multi_response_times(output, n_param_sets)

  Map(
    output_data_list,
    output[["events"]],
    infection,
    event_info[["npi_data"]],
    event_info[["vaccination_data"]],
    f = function(x, y, z, zz, zzz) {
      o <- list(
        ode_soln = x,
        ode_events = y,
        total_time = time_end,
        country = country,
        infection = z,
        vaccination = vaccination,
        behaviour = behaviour,
        response_data = list(
          response_strategy = response_identifier,
          openness = get_data(npi[[1]], "openness"), # from first NPI
          npi_info = zz,
          vaccination_info = zzz
        )
      )

      as_daedalus_output(o)
    }
  )
}

#' Split a dust2 solution with multiple groups
#'
#' @description
#' Creates a list of solutions similar to output from a single group run.
#'
#' @param x A grouped dust2 output.
#'
#' @keywords internal
split_multi_soln <- function(x) {
  ode_data <- x[["data"]]
  comp_names <- names(ode_data)

  data_list <- lapply(ode_data, function(le) {
    max_dim <- length(dim(le))
    asplit(le, max_dim - 1)
  })

  data_list <- data.table::transpose(data_list)

  lapply(data_list, function(le) {
    names(le) <- comp_names
    le
  })
}
