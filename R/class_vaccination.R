#' Constructor for the `<daedalus_vaccination>` class
#'
#' @description Internal constructor function to make new objects of class
#' `<daedalus_vaccination>`. See [daedalus_vaccination()] for the user-facing
#' helper function which calls this function internally.
#'
#' @param parameters A named list of parameters for the vaccination scenario.
#' These must be `start_time` (single numeric), `rate` (single numeric),
#' `uptake_limit` (single numeric), `country`
#' (coercible to `<daedalus_country>`), `efficacy` (single numeric), and
#' `waning_period` (single numeric).
#'
#' @param `...` Other parameters passed to `new_daedalus_response()`.
#'
#' @return An object of the `<daedalus_vaccination>` class, which inherits from
#' the `<daedalus_response>` super-class.
#'
#' @keywords internal
#'
#' @noRd
new_daedalus_vaccination <- function(parameters, ...) {
  new_daedalus_response(
    name = "vaccination", # used by daedalus::response class to gen event names
    parameters,
    class = "daedalus_vaccination",
    ...
  )
}

#' Convert an uptake limit from a percentage to a number
#'
#' @inheritParams class_vaccination
#'
#' @return A single number giving the total number of individuals expected to
#' take up vaccination. If `country` is `NULL`, the function returns `NULL`.
#'
#' @keywords internal
uptake_percent_to_number <- function(uptake_limit, country) {
  sum(get_data(country, "demography")) * uptake_limit / 100.0
}

#' Represent vaccine investment scenarios for DAEDALUS
#'
#' @name class_vaccination
#' @rdname class_vaccination
#'
#' @description Helper functions to create and work with S3 class
#' `<daedalus_vaccination>` objects for use with [daedalus()].
#' These objects store vaccination parameters for reuse and have methods for
#' easy parameter access and editing, as well as processing raw vaccination
#' characteristics for the DAEDALUS model.
#'
#' @param name A vaccination investment scenario name from among
#' [daedalus.data::vaccination_scenario_names].
#' Selecting an epidemic automatically pulls in vaccination parameters
#' associated with the epidemic; these are stored as packaged data in
#' `daedalus.data::vaccination_scenario_data`.
#'
#' @param start_time The number of days after the start of the epidemic that
#' vaccination begins. Must be a single number. Defaults to `NULL` and the start
#' time is taken from the vaccination scenarios specified by `name`. Passed to
#' the `time_on` argument in [new_daedalus_response()] via the class constructor
#' `new_daedalus_vaccination()`.
#'
#' @param rate A single number for the _percentage_ of the total population that
#' can be vaccinated each day. This is converted into a proportion automatically
#' within [daedalus()].
#'
#' @param uptake_limit A single number giving the upper limit for the
#' _percentage_ of the population that can be vaccinated. When this limit is
#' reached, vaccination ends. Passed to the `value_state_off` argument in
#' [new_daedalus_response()] via the class constructor
#' `new_daedalus_vaccination()`.
#'
#' @param country A `<daedalus_country>` object or a 2- or 3-character string
#' that can be coerced to a `<daedalus_country>` (e.g. `"GBR"` for the United
#' Kingdom). Used to determine when vaccination should end.
#'
#' @param efficacy A single number in the range `[0, 100]` giving the efficacy
#' of vaccination in preventing infection. A value of 0 indicates that
#' vaccinated individuals are as susceptible to infection as unvaccinated ones,
#' while 100 would indicate completely non-leaky vaccination that completely
#' protects against infection.
#'
#' @param waning_period A single number representing the number of days over
#' which the average individual wanes out of the vaccinated stratum to the
#' unvaccinated stratum. Only individuals in the susceptible and recovered
#' compartments can wane out of being vaccinated.
#'
#' @param x An object to be tested or printed as a `<daedalus_vaccination>`.
#'
#' @param ... For `daedalus_vaccination()`, other parameters passed to
#' [new_daedalus_response()].
#' For the `print` method, other parameters passed to [print()].
#'
#' @details
#' Note that vaccination once ended by reaching the `uptake_limit` does not
#' restart once individuals wane out of the vaccinated compartment.
#'
#' @export
#'
#' @examples
#' # for no advance vaccine investment in the UK
#' daedalus_vaccination("none", "GBR")
#'
#' # modifying parameters during initialisation
#' # set daily vaccination rate to 1.5% of population
#' daedalus_vaccination("low", "GBR", rate = 1.5)
daedalus_vaccination <- function(
  name,
  country,
  start_time = NULL,
  rate = NULL,
  uptake_limit = NULL,
  efficacy = 50,
  waning_period = 180
) {
  # input checking -- currently we do not allow flexibility in
  # naming vaccine scenarios
  name <- rlang::arg_match(name, daedalus.data::vaccination_scenario_names)

  checkmate::assert_integerish(start_time, lower = 0, null.ok = TRUE)

  # allow conversion of an uptake limit from a percentage to an absolute number
  country <- daedalus_country(country)

  checkmate::assert_number(
    rate,
    null.ok = TRUE,
    finite = TRUE,
    lower = 0,
    upper = 10 # arbitrary upper-limit on daily vaccination rate
  )
  checkmate::assert_number(efficacy, lower = 0, upper = 100, null.ok = TRUE)

  # filter out NULLs for optional parameters
  user_params <- list(
    start_time = start_time,
    rate = rate,
    uptake_limit = uptake_limit,
    efficacy = efficacy,
    waning_period = waning_period
  )
  user_params <- Filter(Negate(is.null), user_params)

  params <- daedalus.data::vaccination_scenario_data[[name]]
  params[names(user_params)] <- user_params

  x <- new_daedalus_vaccination(
    params,
    identifier = name,
    id_flag = get_flag_index("vax_flag", country),
    time_on = params[["start_time"]],
    id_state_off = get_state_indices("new_vax", country),
    value_state_off = uptake_percent_to_number(
      params[["uptake_limit"]],
      country
    ),
    root_state_on = 1L, # meaningless as id_state_on is NA
    root_state_off = 1L, # increasing root on total new vaccinations
    id_time_log = get_flag_index("vax_start_time", country)
  )

  validate_daedalus_vaccination(x)

  x
}

#' Validator for the `<daedalus_vaccination>` class
#'
#' @param x An object to be validated as a `<daedalus_vaccination>` object.
#'
#' @keywords internal
#'
#' @noRd
validate_daedalus_vaccination <- function(x) {
  if (!is_daedalus_vaccination(x)) {
    cli::cli_abort(
      "Object should be of class {.cls daedalus_vaccination};
      check class assignment."
    )
  }

  # check class members
  expected_fields <- daedalus.data::vaccination_parameter_names
  has_fields <- checkmate::test_names(
    attr(x$parameters, "names"),
    permutation.of = expected_fields
  )
  if (!has_fields) {
    cli::cli_abort(
      "`x` is class {.cls daedalus_vaccination} but does not have the correct
      attributes"
    )
  }

  invisible(lapply(daedalus.data::vaccination_parameter_names, function(n) {
    lgl <- checkmate::test_number(x$parameters[[n]], lower = 0.0, finite = TRUE)
    if (!lgl) {
      cli::cli_abort(
        "<daedalus_vaccination> member {.str {n}} must be a single finite
          positive number"
      )
    }
  }))

  invisible(x)
}

#' Check if an object is a `<daedalus_vaccination>`
#' @name class_vaccination
#'
#' @export
is_daedalus_vaccination <- function(x) {
  inherits(x, "daedalus_vaccination")
}

#' Print a `<daedalus_vaccination>` object
#'
#' @name class_vaccination
#'
#' @export
print.daedalus_vaccination <- function(x, ...) {
  validate_daedalus_vaccination(x)
  format(x, ...)
}

#' Format a `<daedalus_vaccination>` object
#'
#' @param x A `<daedalus_vaccination>` object.
#'
#' @param ... Other arguments passed to [format()].
#'
#' @return Invisibly returns the `<daedalus_vaccination>` object `x`.
#' Called for printing side-effects.
#'
#' @keywords internal
#'
#' @noRd
format.daedalus_vaccination <- function(x, ...) {
  chkDots(...)

  cli::cli_text("{.cls {class(x)}}")
  cli::cli_text("Vaccine investment scenario: {cli::style_bold(x$identifier)}")
  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
  cli::cli_bullets(
    class = divid,
    c(
      "*" = "Start time (days): {.val {x$parameters$start_time}}",
      "*" = "Rate (% per day): {.val {x$parameters$rate}}",
      "*" = "Uptake limit (%): {x$parameters$uptake_limit}",
      "*" = "Efficacy (%): {x$parameters$efficacy}",
      "*" = "Waning period (mean, days): {x$parameters$waning_period}"
    )
  )
  cli::cli_end(divid)

  invisible(x)
}

#' @name get_data
#' @export
get_data.daedalus_vaccination <- function(x, to_get, ...) {
  chkDots(...)
  validate_daedalus_vaccination(x)

  good_to_get <- checkmate::test_string(to_get) &&
    checkmate::test_subset(to_get, names(x$parameters))

  if (!good_to_get) {
    cli::cli_abort(c(
      "`to_get` must be a string available in the {.cls class(x)}",
      i = "Allowed values are {.str {names(x)}}"
    ))
  }

  x$parameters[[to_get]]
}

#' @name set_data
#' @export
set_data.daedalus_vaccination <- function(x, ...) {
  to_set <- rlang::list2(...)
  checkmate::assert_list(to_set, "numeric", any.missing = FALSE)

  is_good_subs <- checkmate::test_subset(
    names(to_set),
    daedalus.data::vaccination_parameter_names
  )
  if (!is_good_subs) {
    cli::cli_abort(
      "Found a disallowed parameter substitution in `set_data()`: you are
      trying to set a vaccination parameter that is not supported by the
      DAEDALUS model."
    )
  }

  x$parameters[names(to_set)] <- to_set

  validate_daedalus_vaccination(x)

  x
}

#' Prepare vaccination parameters for model
#'
#' @name prepare_parameters
#'
#' @keywords internal
prepare_parameters.daedalus_vaccination <- function(x, ...) {
  chkDots(...)
  validate_daedalus_vaccination(x)

  # only need rates for nu and psi for now
  list(
    nu = get_data(x, "rate") / 100,
    psi = 1 / get_data(x, "waning_period"),
    uptake_limit = get_data(x, "uptake_limit") / 100,
    vax_start_time = get_data(x, "start_time")
  )
}

#' Dummy vaccination
#'
#' The efficacy of a dummy vaccination object is set to 50% as a stop-gap
#' implementation of pre-existing population immunity. In scenarios where a
#' 'true' vaccination scenario is to be passed, it doesn't matter. In scenarios
#' where no vaccination is intended, it allows any individuals with pre-existing
#' immunity to be only partially susceptible, while still preventing any
#' model-time vaccinations as the `rate` is set to 0.
#'
#' @return A `daedalus_vaccination` object intended to have no effect;
#' vaccination rate and efficacy are set to zero.
#'
#' @keywords internal
dummy_vaccination <- function(country) {
  # a dummy vaccination with rates and start set to zero
  params <- list(
    rate = 0,
    efficacy = 50, # set > 0 for any pre-existing immunity in initial_state
    start_time = 0,
    uptake_limit = 0,
    waning_period = 1
  )
  x <- new_daedalus_vaccination(
    params,
    identifier = "no vaccination",
    id_flag = get_flag_index("vax_flag", country),
    root_state_on = 1L,
    root_state_off = 1L,
    id_time_log = get_flag_index("vax_start_time", country)
  )
  validate_daedalus_vaccination(x)
  x
}

#' Validate vaccination inputs
#'
#' @param x An object to be validated as input to the `vaccine_investment`
#' argument of [daedalus()].
#'
#' @inheritParams daedalus
#'
#' @keywords internal
#'
#' @return A `<daedalus_vaccination>` object.
validate_vaccination_input <- function(x, country) {
  is_good_class <- checkmate::test_multi_class(
    x,
    c("daedalus_vaccination", "character"),
    null.ok = TRUE
  )

  if (!is_good_class) {
    cli::cli_abort(
      "daedalus: Got an unexpected value of class {.cls {class(x)}} \
      for `vaccine_investment`; it may only be `NULL`, \
      `<daedalus_vaccination>`, or a string giving the name of a pre-defined \
      vaccination strategy."
    )
  }

  if (is_daedalus_vaccination(x)) {
    invisible(x)
  } else if (is.character(x)) {
    x <- rlang::arg_match(
      x,
      daedalus.data::vaccination_scenario_names
    )
    x <- daedalus_vaccination(x, country = country)

    x
  } else {
    dummy_vaccination(country)
  }
}

#' Prepare susceptibility matrix for a vaccine-country pair
#'
#' @description
#' Helper function to prepare a susceptibility matrix to be used internally to
#' modulate the number of infections in vaccinated groups.
#'
#' @inheritParams daedalus
#'
#' @returns A matrix with dimensions as follows:
#' - Rows: number of age and economic groups in `country`;
#' - Cols: number of vaccination strata in the DAEDALUS model, given as
#' `N_VAX_STRATA`.
#'
#' @keywords internal
make_susc_matrix <- function(vaccination, country) {
  # no input checks for this internal function
  efficacy <- get_data(vaccination, "efficacy")
  tau <- 1.0 - efficacy / 100.0

  n_strata <- get_data(country, "n_strata")

  # default assumption is full susceptibility
  susc_matrix <- matrix(1, n_strata, N_VACCINE_STRATA)
  susc_matrix[, i_VACCINATED_STRATUM] <- rep(tau, n_strata)

  susc_matrix
}
