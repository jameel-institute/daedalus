#' Prepare a deaths-based behaviour-change mechanism
#'
#' @name class_behaviour
#' @rdname class_behaviour
#'
#' @description
#' Helper functions to prepare `<daedalus_behaviour>` class responses
#' corresponding to either old or new behavioural change mechanisms.
#'
#' @param rate
#'
#' @param lower_limit
#' 
#' @details
#' ## Details: Behavioural models
#'
#' There are three possible options for this behavioural module, given below.
#' **Note that** a major issue with including this in a model run (any value
#' other than `"off"`) is that it leads to substantially lower response costs,
#' and generally better health outcomes (lives lost), **without accounting for
#' any attendant economic or social costs**. As such, please treat this option
#' as **highly experimental**.
#'
#' - `"off"`: There is no effect of daily deaths on infection transmissibility.
#' This is the **default choice**.
#'
#' - `"independent"`: Public-concern over deaths reduces transmissibility, and
#' is independent of any mandated responses. It begins at the start of the
#' simulation, and continues until the simulation ends.
#'
#' - `"npi_linked`": Public-concern over deaths reduces transmissibility, but
#' only when a  mandated response is active. Note that there is currently no way
#' to end a response triggered by a state, i.e., an NPI launched due to hospital
#' capacity being breached.
daedalus_old_behaviour <- function(rate = 0.001, lower_limit = 0.2) {
  new_daedalus_response(
    name = "behaviour",
    identifier = "old_behaviour",
    # no flag index provided as none needed
    parameters = list(
      rate = rate,
      lower_limit = lower_limit
    ),
    class = "daedalus_behaviour"
  )
}

#' @name class_behaviour
#'
#' @param hospital_capacity
#'
#' @param behav_effectiveness
#'
#' @param baseline_optimism
#'
#' @param responsiveness
#'
#' @param k0
#'
#' @param k1
daedalus_new_behaviour <- function(
  hospital_capacity,
  behav_effectiveness = 0.5,
  baseline_optimism = 0.5,
  responsiveness = 1.5,
  k0 = 4.59,
  k1 = -9.19
) {
  # TODO: input checking

  if (is_daedalus_country(hospital_capacity)) {
    hospital_capacity <- get_data(hospital_capacity, "hospital_capacity")
  }

  # note that this order is important and differs from the argument order
  # due to R argument convention (default valued args after others)
  parameters <- list(
    hospital_capacity = hospital_capacity,
    behav_effectiveness = behav_effectiveness,
    baseline_optimism = baseline_optimism,
    k0 = k0,
    k1 = k1,
    responsiveness = responsiveness # k2
  )

  new_daedalus_response(
    name = "behaviour",
    identifier = "new_behaviour",
    # no flag index provided as none needed
    parameters = parameters,
    class = "daedalus_behaviour"
  )
}

#' Check if an object is a `<daedalus_behaviour>`
#' @name class_behaviour
#'
#' @export
is_daedalus_behaviour <- function(x) {
  inherits(x, "daedalus_behaviour")
}

#' Validator for the `<daedalus_behaviour>` class
#'
#' @param x An object to be validated as a `<daedalus_behaviour>` object.
#'
#' @keywords internal
#'
#' @noRd
validate_daedalus_behaviour <- function(x) {
  if (!is_daedalus_behaviour(x)) {
    cli::cli_abort(
      "Object should be of class {.cls daedalus_behaviour};
      check class assignment."
    )
  }

  expected_identifiers <- c("no_behaviour", "old_behaviour", "new_behaviour")
  has_good_identifier <- checkmate::test_scalar(x$identifier) &&
    checkmate::test_subset(x$identifier, expected_identifiers)

  if (!has_good_identifier) {
    cli::cli_abort(
      "Got a {.cls {class(x)}} but it does not have an accepted identifier, or
      has more than one `identifier`;
      these may only be one of {.str {expected_identifiers}}."
    )
  }

  # WIP: more to come
  expected_fields_old <- c(
    "rate",
    "lower_limit"
  )
  expected_fields_new <- c(
    "hospital_capacity",
    "behav_effectiveness",
    "baseline_optimism",
    "k0",
    "k1",
    "responsiveness"
  )

  if (x$identifier == "old_behaviour") {
    checkmate::assert_names(
      attr(x$parameters, "names"),
      subset.of = expected_fields_old
    )
  } else if (x$identifier == "new_behaviour") {
    checkmate::assert_names(
      attr(x$parameters, "names"),
      subset.of = expected_fields_new
    )
  }

  invisible(x)
}

#' Print a `<daedalus_behaviour>` object
#'
#' @name class_behaviour
#'
#' @export
print.daedalus_behaviour <- function(x, ...) {
  validate_daedalus_behaviour(x)
  format(x, ...)
}

#' Format a `<daedalus_behaviour>` object
#'
#' @param x A `<daedalus_behaviour>` object.
#'
#' @param ... Other arguments passed to [format()].
#'
#' @return Invisibly returns the `<daedalus_behaviour>` object `x`.
#' Called for printing side-effects.
#'
#' @keywords internal
#'
#' @noRd
format.daedalus_behaviour <- function(x, ...) {
  chkDots(...)

  cli::cli_text("{.cls {class(x)}}")
  cli::cli_text("Behaviour model: {cli::style_bold(x$identifier)}")

  cli::cli_dl(
    x$parameters
  )

  invisible(x)
}

dummy_behaviour <- function() {
  new_daedalus_response(
    name = "behaviour",
    identifier = "no_behaviour",
    parameters = list(lower_limit = 1.0), # a dummy value, never used
    class = "daedalus_behaviour"
  )
}

#' Prepare infection parameters for model
#'
#' @name prepare_parameters
#'
#' @keywords internal
prepare_parameters.daedalus_behaviour <- function(x) {
  validate_daedalus_behaviour(x)

  behav_enum <- switch(
    x$identifier,
    no_behaviour = 0L,
    old_behaviour = 1L,
    new_behaviour = 2L
  )
  behav_params <- unlist(x$parameters)
  list(
    behav_enum = behav_enum,
    behav_params = behav_params
  )
}

validate_behaviour_input <- function(x) {
  is_good_class <- checkmate::test_class(
    x,
    "daedalus_behaviour",
    null.ok = TRUE
  )
  if (!is_good_class) {
    cli::cli_abort(
      "daedalus: Got an unexpected value of class {.cls {class(x)}} \
      for `behaviour`; it may only be `NULL` or `<daedalus_behaviour>`."
    )
  }

  if (is_daedalus_behaviour(x)) {
    x
  } else {
    dummy_behaviour()
  }
}
