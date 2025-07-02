#' Constructor for the <daedalus_response> super-class and sub-classes
#'
#' @name class_response
#' @rdname class_response
#'
#' @param name Response name. This will eventually be passed to the name
#' argument in the C++ class `daedalus::events::response`. No default for the
#' super-class constructor; each sub-class has a fixed name to aid
#' identifiability in `dust2` events outputs.
#'
#' @param class Sub-class name. This is only ever passed from each sub-class
#' constructor. Only a fixed set of values is currently allowed.
#'
#' @param parameters Sub-class parameters passed from sub-class constructors.
#' Defaults to an empty list to aid development.
#'
#' @param time_on Intended to be a numeric vector of start times.
#'
#' @param duration Intended to be a numeric vector of durations.
#'
#' @param id_state_on Intended to be a numeric vector of compartment indices
#' which are checked for roots that when found launch an event. The check is
#' always performed on the _sum_ of the state at the indices.
#'
#' @param value_state_on Intended to be a numeric vector of roots which when
#' found change a switch-like state-flag to 'on'. The check is
#' always performed on the _sum_ of the state at the indices.
#'
#' @param id_state_off Intended to be a numeric vector of compartment indices
#' which are checked for roots that when found end an event.
#'
#' @param value_state_off Intended to be a numeric vector of roots which when
#' found change a switch-like state-flag to 'off'.
#'
#' @param class The name of the sub-class being created. May be one of
#' "daedalus_npi", "daedalus_vaccination", "daedalus_behaviour", or
#' "daedalus_mortality".
#'
#' @param x For all functions taking `x`, must be an object of class
#' `<daedalus_response>`, or an object to be validated as of the class.
#'
#' @return
#' `new_daedalus_response()` returns a `<daedalus_response>`. Since there is no
#' default value to `class`, each `<daedalus_response>` must also be of one of
#' the sub-classes returned by the sub-class constructors:
#'
#' - `new_daedalus_npi()`: `<daedalus_npi>`;
#'
#' - `new_daedalus_vax()`: `<daedalus_vax>`;
#'
#' - `new_daedalus_behaviour()`: `<daedalus_behaviour>`;
#'
#' - `new_daedalus_mortality()`: `<daedalus_mortality>`.
#'
#' @keywords internal
new_daedalus_response <- function(
  name,
  class = response_class_names,
  parameters = list(),
  time_on = NULL,
  duration = NULL,
  id_state_on = NULL,
  value_state_on = NULL,
  id_state_off = NULL,
  value_state_off = NULL
) {
  # class arg empty to force responses to have a sub-class
  class <- rlang::arg_match0(class, response_class_names) # no defaults

  x <- list(
    name = name,
    class = class,
    parameters = parameters,
    time_on = time_on,
    duration = duration,
    id_state_on = id_state_on,
    value_state_on = value_state_on,
    id_state_off = id_state_off,
    value_state_off = value_state_off
  )

  class(x) <- c(class, "daedalus_response")

  validate_daedalus_response(x)

  x
}

#' @name class_response
response_class_names <- c(
  "daedalus_npi",
  "daedalus_vaccination",
  "daedalus_behaviour",
  "daedalus_mortality"
)

#' @name class_response
new_daedalus_npi <- function() {
  new_daedalus_response("npi", "daedalus_npi", list())
}

#' @name class_response
new_daedalus_behaviour <- function() {
  new_daedalus_response("behaviour", "daedalus_behaviour", list())
}

#' @name class_response
new_daedalus_mortality <- function() {
  new_daedalus_response("mortality", "daedalus_mortality", list())
}

#' @name class_response
is_daedalus_response <- function(x) {
  inherits(x, "daedalus_response")
}

#' @name class_response
validate_daedalus_response <- function(x) {
  if (!is_daedalus_response(x)) {
    cli::cli_abort(
      "Object should be of class {.cls daedalus_response};
      check class assignment."
    )
  }

  # check class members apart from parameters
  expected_fields <- c(
    "name",
    "class",
    "parameters",
    "time_on",
    "duration",
    "id_state_on",
    "value_state_on",
    "id_state_off",
    "value_state_off"
  )

  assert_class_fields(x, expected_fields)

  # no check on name for now; parameters should be checked in sub-classes
  if (!checkmate::test_list(x$parameters)) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str parameters} must be a list but\
      is not."
    )
  }

  is_good_time_on <- checkmate::test_integerish(
    x$time_on,
    lower = 0,
    any.missing = FALSE,
    null.ok = TRUE
  )
  if (!is_good_time_on) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str time_on} must be a vector of\
      integer-ish numbers, but it is not."
    )
  }

  is_good_duration <- checkmate::test_integerish(
    x$duration,
    lower = 0,
    any.missing = FALSE,
    len = length(x$time_on),
    null.ok = TRUE
  )
  if (!is_good_duration) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str duration} must be a vector of\
      integer-ish numbers of the same length as `time_on`, but it is not."
    )
  }

  # NOTE: possibly needs an upper bound?
  # NOTE: may need to be a list allowing integer vectors if an event is
  # expected to be triggered by more than one state being reached
  is_good_state_on <- checkmate::test_integerish(
    x$id_state_on,
    lower = 0,
    any.missing = FALSE,
    null.ok = TRUE
  )
  if (!is_good_state_on) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str id_state_on} must be a vector of\
      integer-ish numbers, but it is not."
    )
  }

  is_good_state_off <- checkmate::test_integerish(
    x$id_state_off,
    lower = 0,
    any.missing = FALSE,
    null.ok = TRUE
  )
  if (!is_good_state_off) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str id_state_off} must be a vector\
      of integer-ish numbers, but it is not."
    )
  }

  # NOTE: no negative values expected
  is_good_value_on <- checkmate::test_numeric(
    x$value_state_on,
    lower = 0.0,
    any.missing = FALSE,
    null.ok = TRUE
  )
  if (!is_good_value_on) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str value_state_on} must be a\
      numeric vector with no missing values allowed, but it is not."
    )
  }

  is_good_value_off <- checkmate::test_numeric(
    x$value_state_off,
    lower = 0.0,
    any.missing = FALSE,
    null.ok = TRUE
  )
  if (!is_good_value_off) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str value_state_off} must be a\
      numeric vector with no missing values allowed, but it is not."
    )
  }

  invisible(x)
}
