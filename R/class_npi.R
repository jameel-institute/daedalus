#' Constructor for the `<daedalus_npi>` class
#'
#' @description Internal constructor function to make new objects of class
#' `<daedalus_npi>`. See [daedalus_npi()] for the user-facing
#' helper function which calls this function internally.
#'
#' @param parameters A named list of parameters for the NPI strategy.
#' These must be `start_time` (single numeric), `rate` (single numeric),
#' `uptake_limit` (single numeric), `country`
#' (coercible to `<daedalus_country>`), `efficacy` (single numeric), and
#' `waning_period` (single numeric).
#'
#' @param `...` Other parameters passed to `new_daedalus_response()`.
#'
#' @return An object of the `<daedalus_npi>` class, which inherits from
#' the `<daedalus_response>` super-class.
#'
#' @keywords internal
#'
#' @noRd
new_daedalus_npi <- function(parameters, ...) {
  new_daedalus_response(
    name = "npi",
    parameters,
    class = "daedalus_npi",
    ...
  )
}

#' Represent non-pharmaceutical intervention strategies for DAEDALUS
#'
#' @name class_npi
#' @rdname class_npi
#'
#' @description Helper functions to create and work with S3 class
#' `<daedalus_npi>` objects for use with [daedalus()].
#' These objects store NPI parameters for reuse and have methods for
#' easy parameter access and editing, as well as processing raw NPI
#' characteristics for the DAEDALUS model.
#'
#' All NPIs must be initialised with an associated `country` and `infection`;
#' these are used to determine the state-values (hospital capacity) and
#' incidence-prevalence ratio (IPR) at which the NPI starts and ends reactively.
#'
#' @inheritParams daedalus
#'
#' @param name An NPI strategy name from among
#' [daedalus.data::closure_strategy_names], or `NA`.
#' Passing a pre-defined strategy name automatically pulls in openness
#' parameters for the associated response strategy; these are stored as
#' packaged data in `daedalus.data::closure_strategy_data`.
#' Pass `NA` to define a custom response strategy by passing a vector to
#' `openness`.
#'
#' @param openness An optional numeric vector where all values are in the range
#' \eqn{[0, 1]}, giving the openness of each economic sector in the model.
#' Expected to have a length of `N_ECON_SECTORS` (currently 45).
#'
#' @param start_time The number of days after the start of the epidemic that
#' the NPI response begins. Must be a single number. Defaults to 30. Passed to
#' the `time_on` argument in [new_daedalus_response()] via the class constructor
#' `new_daedalus_npi()`.
#'
#' @param duration The maximum number of days that and NPI response is active,
#' whether started by passing the `start_time` or when a state threshold is
#' crossed. Defaults to 60 days.
#'
#' @param x An object to be tested or printed as a `<daedalus_npi>`.
#'
#' @param ... For `daedalus_npi()`, other parameters passed to
#' [new_daedalus_response()].
#' For the `print` method, other parameters passed to [print()].
#'
#' @details
#' Note that NPIs are reactive to the model state (i.e., the epidemic state),
#' and can trigger when state conditions are met.
#'
#' @export
#'
#' @examples
#' # for a school closure strategy
#' daedalus_npi("school_closures", "GBR", "sars_cov_1")
#'
#' # set custom openness
#' daedalus_npi(
#'   NA,
#'   "GBR", "sars_cov_1",
#'   openness = rep(0.1, 45)
#' )
daedalus_npi <- function(
  name,
  country,
  infection,
  openness = NULL,
  start_time = 30,
  duration = 60
) {
  # input checking
  if (is.na(name)) {
    identifier <- "custom"
    # check openness
    is_good_openness <- checkmate::test_numeric(
      openness,
      lower = 0.0,
      upper = 1.0,
      any.missing = FALSE,
      len = N_ECON_SECTORS
    )

    if (!is_good_openness) {
      cli::cli_abort(
        "<daedalus_npi> parameter {.str openness} must be a numeric of length \
        {N_ECON_SECTORS}, with values between 0.0 and 1.0."
      )
    }
  } else {
    # prevent users from passing both a predefined strategy and custom openness
    name <- rlang::arg_match(name, daedalus.data::closure_strategy_names)
    identifier <- name

    if (!is.null(openness)) {
      cli::cli_abort(
        "daedalus_npi: Got both a pre-defined NPI strategy passed to `name`
        and custom `openness` values. Pass `NA` to `name` to use custom
        openness values."
      )
    }

    openness <- daedalus.data::closure_strategy_data[[name]]
  }

  country <- validate_country_input(country)
  infection <- validate_infection_input(infection)

  checkmate::assert_integerish(start_time, lower = 0, null.ok = TRUE)
  checkmate::assert_integerish(duration, lower = 0, null.ok = TRUE)

  params <- list(openness = openness)

  # NOTE: npis start on hospital capacity, but this can be extended
  x <- new_daedalus_npi(
    params,
    identifier = identifier,
    id_flag = get_flag_index("npi_flag", country),
    time_on = start_time,
    duration = duration,
    id_state_on = get_state_indices("hospitalised", country),
    id_state_off = get_state_indices("ipr", country),
    value_state_on = get_data(country, "hospital_capacity"),
    value_state_off = get_data(infection, "gamma_Is"),
    id_time_log = get_flag_index("npi_start_time", country)
  )

  validate_daedalus_npi(x)

  x
}

#' Validator for the `<daedalus_npi>` class
#'
#' @param x An object to be validated as a `<daedalus_npi>` object.
#'
#' @keywords internal
#'
#' @noRd
validate_daedalus_npi <- function(x) {
  if (!is_daedalus_npi(x)) {
    cli::cli_abort(
      "Object should be of class {.cls daedalus_npi};
      check class assignment."
    )
  }

  # check class members
  expected_fields <- "openness"
  has_fields <- checkmate::test_names(
    attr(x$parameters, "names"),
    permutation.of = expected_fields
  )
  if (!has_fields) {
    cli::cli_abort(
      "`x` is class {.cls daedalus_npi} but does not have the correct \
      attributes."
    )
  }

  # check openness
  is_good_openness <- checkmate::test_numeric(
    x$parameters$openness,
    lower = 0.0,
    upper = 1.0,
    any.missing = FALSE,
    len = N_ECON_SECTORS
  )
  if (!is_good_openness) {
    cli::cli_abort(
      "<daedalus_npi> parameter {.str openness} must be a numeric of length
    {N_ECON_SECTORS}, with values between 0.0 and 1.0."
    )
  }

  invisible(
    lapply(daedalus.data::vaccination_parameter_names, function(n) {
      lgl <- checkmate::test_number(
        x$parameters[[n]],
        lower = 0.0,
        finite = TRUE,
        na.ok = TRUE
      )
      if (!lgl) {
      }
    })
  )

  invisible(x)
}

#' Check if an object is a `<daedalus_npi>`
#' @name class_npi
#'
#' @export
is_daedalus_npi <- function(x) {
  inherits(x, "daedalus_npi")
}

#' Print a `<daedalus_npi>` object
#'
#' @name class_npi
#'
#' @export
print.daedalus_npi <- function(x, ...) {
  validate_daedalus_npi(x)
  format(x, ...)
}

#' Format a `<daedalus_npi>` object
#'
#' @param x A `<daedalus_npi>` object.
#'
#' @param ... Other arguments passed to [format()].
#'
#' @return Invisibly returns the `<daedalus_npi>` object `x`.
#' Called for printing side-effects.
#'
#' @keywords internal
#'
#' @noRd
format.daedalus_npi <- function(x, ...) {
  chkDots(...)

  cli::cli_text("{.cls {class(x)}}")
  cli::cli_text("NPI strategy: {cli::style_bold(x$identifier)}")
  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
  cli::cli_bullets(
    class = divid,
    c(
      "*" = "Start time (days): {.val {x$time_on}}",
      "*" = "Openness (mean prop.): {.val {mean(x$parameters$openness)}}",
      "*" = "Default duration (days): {.val {x$duration}} "
    )
  )
  cli::cli_end(divid)

  invisible(x)
}

#' @name get_data
#' @export
get_data.daedalus_npi <- function(x, to_get, ...) {
  chkDots(...)
  validate_daedalus_npi(x)

  good_to_get <- checkmate::test_string(to_get) &&
    checkmate::test_subset(to_get, names(x$parameters))

  if (!good_to_get) {
    cli::cli_abort(c(
      "`to_get` must be a string available in the {.cls class(x)}",
      i = "Allowed values are {.str {names(x$parameters)}}"
    ))
  }

  x$parameters[[to_get]]
}

#' Dummy NPI
#'
#' @return A `<daedalus_npi>` object intended to have no effect; openness is
#' set to 1.0, and start time and duration are set to `NULL`.
#'
#' @keywords internal
dummy_npi <- function() {
  # a dummy npi with rates and start set to zero
  params <- list(
    openness = rep(1.0, N_ECON_SECTORS)
  )
  x <- new_daedalus_npi(
    params,
    identifier = "none",
    id_flag = NA_integer_,
    root_state_on = 1L,
    root_state_off = 1L,
    id_time_log = 1L # NOTE: this is never used as NPI is never switched on
  )
  validate_daedalus_npi(x)
  x
}

#' Validate npi inputs
#'
#' @param x An object to be validated as input to the `vaccine_investment`
#' argument of [daedalus()].
#'
#' @inheritParams daedalus
#'
#' @keywords internal
#'
#' @return A `<daedalus_npi>` object.
validate_npi_input <- function(
  x,
  country,
  infection,
  response_time,
  response_duration
) {
  is_good_class <- checkmate::test_multi_class(
    x,
    c("daedalus_npi", "character", "numeric"),
    null.ok = TRUE
  )
  if (!is_good_class) {
    cli::cli_abort(
      "daedalus: Got an unexpected value of class {.cls {class(x)}} \
      for `response_strategy`; it may only be `NULL`, `<daedalus_npi>`, a \
      numeric vector or a string giving the name of a pre-defined NPI strategy."
    )
  }

  if (is_daedalus_npi(x)) {
    invisible(x)
  } else if (is.null(x) || identical(x, "none")) {
    dummy_npi()
  } else if (is.character(x)) {
    x <- rlang::arg_match(
      x,
      daedalus.data::closure_strategy_names,
      error_arg = "response_strategy",
      error_call = parent.frame()
    )

    daedalus_npi(
      x,
      country,
      infection,
      start_time = response_time,
      duration = response_duration
    )
  } else {
    # numeric case
    z <- daedalus_npi(
      NA_character_,
      country,
      infection,
      x,
      response_time,
      response_duration
    )

    z
  }
}

#' @name get_data
#' @export
get_data.daedalus_npi <- function(x, to_get, ...) {
  chkDots(...)
  validate_daedalus_npi(x)

  good_to_get <- checkmate::test_string(to_get) &&
    checkmate::test_subset(to_get, names(x$parameters))

  if (!good_to_get) {
    cli::cli_abort(c(
      "`to_get` must be a string available in the {.cls {class(x)}}",
      i = "Allowed values are {.str {names(x$parameters)}}"
    ))
  }

  x$parameters[[to_get]]
}
