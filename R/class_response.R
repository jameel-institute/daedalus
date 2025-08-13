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
#' @param class The name of the sub-class being created. May be one of
#' "daedalus_npi", "daedalus_vaccination", "daedalus_behaviour", or
#' "daedalus_mortality". No default argument, users are forced to specify a
#' class when building off this constructor.
#'
#' @param identifier An optional string giving the name of a pre-defined
#' response strategy. Sub-class constructors and helper functions provide the
#' names and handle cases where the strategy is a custom one.
#'
#' @param parameters Sub-class parameters passed from sub-class constructors.
#' Defaults to an empty list to aid development.
#'
#' @param id_flag Index for the state-flag that should be changed when this
#' response is on or off. Typically auto-calculated by sub-class constructor or
#' helper functions; see e.g. [daedalus_vaccination()].
#'
#' @param time_on Intended to be a numeric vector of start times.
#'
#' @param time_off Intended to be a numeric vector of end times.
#'
#' @param max_duration Intended to be a number for the maximum duration.
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
#' @param root_state_on A single integer, either 1 or -1, for whether
#' state-dependent launch is on an increasing or decreasing root, respectively.
#' Defaults to 1 for an increasing root as this is the most common use case.
#'
#' @param root_state_off A single integer, either 1 or -1, for whether
#' state-dependent end is on an increasing or decreasing root, respectively.
#' Defaults to -1 for a decreasing root as this is the most common use case.
#'
#' @param id_time_log Intended to be a single number for the state index where
#' the start-time of this response is stored. See also [initial_flags()]. This
#' attribute is intended solely to enable time-limitation on state-triggers.
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
#' - `new_daedalus_hosp_overflow()`: `<daedalus_hosp_overflow>`. This function
#' is not exposed to users.
#'
#' @keywords internal
new_daedalus_response <- function(
  name,
  class = response_class_names,
  parameters = list(),
  identifier = NA_character_,
  id_flag = NA_integer_,
  time_on = NA_real_,
  time_off = NA_real_,
  max_duration = NA_real_,
  id_state_on = NA_integer_,
  value_state_on = NA_real_,
  id_state_off = NA_integer_,
  value_state_off = NA_real_,
  root_state_on = 1L,
  root_state_off = -1L,
  id_time_log = NA_integer_
) {
  # class arg empty to force responses to have a sub-class
  class <- rlang::arg_match0(class, response_class_names) # no defaults

  x <- list(
    name = name,
    class = class,
    identifier = identifier,
    parameters = parameters,
    id_flag = id_flag,
    time_on = time_on,
    time_off = time_off,
    max_duration = max_duration,
    id_state_on = id_state_on,
    value_state_on = value_state_on,
    id_state_off = id_state_off,
    value_state_off = value_state_off,
    root_state_on = root_state_on,
    root_state_off = root_state_off,
    id_time_log = id_time_log
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
  "daedalus_hosp_overflow"
)

#' @name class_response
new_daedalus_behaviour <- function() {
  new_daedalus_response("behaviour", "daedalus_behaviour")
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
    "identifier",
    "parameters",
    "id_flag",
    "time_on",
    "time_off",
    "max_duration",
    "id_state_on",
    "value_state_on",
    "id_state_off",
    "value_state_off",
    "root_state_on",
    "root_state_off",
    "id_time_log"
  )

  assert_class_fields(x, expected_fields)

  # no check on name for now; parameters should be checked in sub-classes
  if (!checkmate::test_list(x$parameters)) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str parameters} must be a list but
      is not."
    )
  }

  is_good_time_on <- checkmate::test_integerish(
    x$time_on,
    lower = 0,
    any.missing = TRUE
  )
  if (!is_good_time_on) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str time_on} must be a vector of
      integer-ish numbers, but it is a {.cls {class(x$time_on)}}"
    )
  }

  is_good_time_off <- checkmate::test_integerish(
    x$time_off,
    lower = 0,
    len = length(x$time_on),
    any.missing = TRUE
  )
  if (!is_good_time_off) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str time_off} must be a vector of
      integer-ish numbers of length {length(x$time_on)}, but it is a \
      {.cls {class(x$time_on)}} of length {length(x$time_off)}."
    )
  }

  is_good_max_duration <- checkmate::test_count(
    x$max_duration,
    na.ok = TRUE # allow NAs for indefinite events
  )
  if (!is_good_max_duration) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str max_duration} must be a single
      integer-ish number, but it is not."
    )
  }

  # NOTE: possibly needs an upper bound?
  # NOTE: may need to be a list allowing integer vectors if an event is
  # expected to be triggered by more than one state being reached
  is_good_state_on <- checkmate::test_integerish(
    x$id_state_on,
    lower = 0
  )
  if (!is_good_state_on) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str id_state_on} must be a vector of
      integer-ish numbers, but it is not."
    )
  }

  is_good_state_off <- checkmate::test_integerish(
    x$id_state_off,
    lower = 0
  )
  if (!is_good_state_off) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str id_state_off} must be a vector
      of integer-ish numbers, but it is not."
    )
  }

  # NOTE: no negative values expected
  is_good_value_on <- checkmate::test_numeric(
    x$value_state_on,
    lower = 0.0
  )
  if (!is_good_value_on) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str value_state_on} must be a
      numeric vector with no missing values allowed, but it is not."
    )
  }

  is_good_value_off <- checkmate::test_numeric(
    x$value_state_off,
    lower = 0.0
  )
  if (!is_good_value_off) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str value_state_off} must be a
      numeric vector with no missing values allowed, but it is not."
    )
  }

  is_good_value_off <- checkmate::test_numeric(
    x$value_state_off,
    lower = 0.0
  )
  if (!is_good_value_off) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str value_state_off} must be a
      numeric vector with no missing values allowed, but it is not."
    )
  }

  is_good_root_type <- checkmate::test_subset(
    c(x$root_state_on, x$root_state_off),
    c(-1L, 1L),
    empty.ok = FALSE
  )
  if (!is_good_root_type) {
    cli::cli_abort(
      "{.cls daedalus_response} members {.str root_state_on} and
      {.str root_state_off} must be either -1 or 1, and one of them is not."
    )
  }

  # allow nearly any values, but probably needs more checks as use cases
  # become obvious
  is_good_id_time_log <- checkmate::test_count(
    x$id_time_log,
    positive = FALSE,
    na.ok = TRUE
  )
  if (!is_good_id_time_log) {
    cli::cli_abort(
      "{.cls daedalus_response} member {.str id_time_log} must be an
      integer > 0 (no missing values allowed), but it is not."
    )
  }

  invisible(x)
}
