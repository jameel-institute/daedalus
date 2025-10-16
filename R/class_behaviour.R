#' Represent behaviour-change mechanisms in DAEDALUS
#'
#' @name class_behaviour
#' @rdname class_behaviour
#'
#' @description
#' Helper functions to prepare `<daedalus_behaviour>` class responses
#' corresponding to either old or new behavioural change mechanisms.
#' Inherits from `<daedalus_response>` to allow for more control in future over
#' when these mechanisms are active.
#'
#' @param rate The marginal rate of decrease in the scaling factor for each
#' additional daily death in the 'old' behavioural model.
#'
#' @param lower_limit The lower limit of the scaling factor in the 'old'
#' behavioural model; prevents scaling factor from reaching zero.
#'
#' @return An object of class `<daedalus_behaviour>` which inherits from
#' `<daedalus_response>`. This is primarily a list holding behavioural
#' parameters.
#'
#' @details
#'
#' Daedalus currently supports two behavioural models (and the option to have
#' neither model active). These models simulate population-level changes towards
#' adopting behaviours that help reduce the risk of infection, which might be
#' expected during a major disease outbreak.
#'
#' In both models, the transmission rate of the infection \eqn{\beta} is
#' reduced for all transmissions by a scaling factor; the models differ in how
#' the scaling factor is calculated.
#'
#' **Note that** a major issue with including this in a model run (any value
#' other than `"off"`) is that it leads to substantially lower response costs,
#' and generally better health outcomes (lives lost), **without accounting for
#' any attendant economic or social costs**. As such, please treat the
#' behavioural models as experimental.
#'
#' ## Deaths-based public concern model
#'
#' This model existed prior to `daedalus v0.2.25` and is referred to as the
#' `"old"` behavioural model.
#'
#' The scaling factor for \eqn{\beta} in this model is
#' \deqn{
#' (1 - \text{rate})^{\Delta D} \times (1 - L) + L
#' }
#' where \eqn{\text{rate}} is the marginal rate of reduction for each additional
#' death per day (\eqn{\Delta D}), and \eqn{L} is the lower limit of the scaling
#' factor to prevent the FOI going to zero.
#'
#' ## Optimism-responsiveness-effectiveness model
#'
#' This model is referred to as the `"new"` behavioural model, and the scaling
#' factor for \eqn{\beta} is given by
#'
#' \deqn{
#' \mathcal{B}(p_t, \delta) = p_t(1 - \delta)[p_t(1 - \delta) + (1 - p_t)] +
#' (1 - p_t)[p_t(1 - \delta) + (1 - p_t)]
#' }
#'
#' where \eqn{p_t} is the proportion of the population taking protective
#' behaviour and \eqn{\delta} is the effectiveness of the behaviour, both in the
#' range \eqn{[0, 1]}. When either is zero, \eqn{\beta} is not scaled down.
#'
#' \eqn{p_t} is computed during the model run, and is given by
#'
#' \deqn{
#' p_t = \frac{1}{1 + \text{exp}\left\{-\left(k_0 + k_1 \bar B +
#' k_2 \frac{H_t}{\bar H}\right)\right\}}
#' }
#'
#' where \eqn{k_0 = 4.59, k_1 = -9.19, k_2} are scaling parameters.
#' \eqn{k_0, k_1} have constant values that cannot be changed by the user, and
#' these have been chosen to produce a sigmoidal relationship between
#' \eqn{\bar B} and \eqn{p_t}.
#'
#' \eqn{k_2} a constant user-supplied parameter that should be interpreted as
#' population responsiveness to a signal of epidemic severity
#' \eqn{H_t / \bar H}, which is the relative burden on hospital capacity
#' (\eqn{H_t}: hospital demand at time \eqn{t}, and
#' \eqn{\bar H}: emergency hospital capacity).
#'
#' \eqn{\bar B} is a constant user-supplied parameter that captures the baseline
#' population optimism about the outbreak.
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
#' @param hospital_capacity The emergency hospital capacity of a country, but
#' **may also be** a `<daedalus_country>` object from which the hospital
#' capacity is extracted. See \eqn{\bar H} in Details.
#'
#' @param behav_effectiveness A single double value for the effectiveness of
#' adopting behavioural measures against the risk of infection. Expected to be
#' in the range \eqn{[0, 1]}, where 0 represents no effectiveness, and 1
#' represents full effectiveness. See \eqn{\delta} in Details. Defaults to
#' 0.5.
#'
#' @param baseline_optimism A single double value for the baseline optimism
#' about pandemic outcomes. Expected to be in the range \eqn{[0, 1]}.
#' See \eqn{\bar B} in Details. Defaults to 0.5.
#'
#' @param responsiveness A single double value for the population responsiveness
#' to an epidemic signal. Must have a lower value of 0, but the upper bound is
#' open. See \eqn{k_2} in Details. Defaults to 1.5.
#'
#' @param k0 A single, optional double value which is a scaling parameter for
#' the sigmoidal relationship between \eqn{p_t} and `behav_effectiveness`.
#' See \eqn{k_0} in Details. Defaults to 4.59.
#'
#' @param k1 A single, optional double value which is another scaling parameter
#' for the sigmoidal relationship between \eqn{p_t} and `behav_effectiveness`.
#' See \eqn{k_1} in Details. Defaults to -9.19.
#'
#' @export
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
#'
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

#' Internal helper function to prepare parameters
#'
#' @keywords internal
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
    validate_daedalus_behaviour(x)
    x
  } else {
    dummy_behaviour()
  }
}
