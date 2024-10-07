#' Constructor for the `<daedalus_vaccination>` class
#'
#' @description Internal constructor function to make new objects of class
#' `<daedalus_vaccination>`. See [daedalus_vaccination()] for the user-facing
#' helper function which calls this function internally.
#'
#' @param name A vaccination investment scenario name from among
#' [daedalus::vaccine_investment_scenarios].
#' @param parameters A named list of parameters for the vaccination scenario.
#'
#' @return An object of the `<daedalus_vaccination>` class, which inherits from
#' a `<list>`.
#' @keywords internal
#' @noRd
new_daedalus_vaccination <- function(name, parameters) {
  x <- c(
    list(name = name),
    parameters
  )

  class(x) <- "daedalus_vaccination"

  x
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
#' [daedalus::vaccination_scenario_names].
#' Selecting an epidemic automatically pulls in vaccination parameters
#' associated with the epidemic; these are stored as packaged data in
#' `daedalus::vaccination_scenario_data`. Default vaccination parameters can be
#' over-ridden by passing them as a named list to `...`.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Manually specified values for
#' any of the vaccination parameters. See **Details** for which vaccination
#' parameters are supported.
#'
#' @details
#'
#' # Vaccination parameters
#'
#' - `vax_start_time`: The number of days after the start of the epidemic that
#' vaccination begins. Must be a single number.
#'
#' - `nu`: A single number for the percentage of the total population that can
#' be vaccinated each day. This is converted into a proportion automatically
#' within [daedalus()].
#'
#' - `vax_uptake_limit`: A single number giving the upper limit for the
#' percentage of the population that can be vaccinated. When this limit is
#' reached, the vaccination rate `nu` is set to zero.
#'
#' @export
daedalus_vaccination <- function(name, ...) {
  # input checking
  name <- rlang::arg_match(name, daedalus::vaccination_scenario_names)
  parameters <- rlang::list2(...)

  is_empty_list <- checkmate::test_list(parameters, len = 0)

  if (!is_empty_list) {
    has_good_names <- checkmate::test_subset(
      names(parameters), daedalus::vaccination_parameter_names,
      empty.ok = TRUE
    )
    if (!has_good_names) {
      cli::cli_abort(
        "Found unexpected values in `...`; the only allowed parameters are:
        {.str {daedalus::vaccination_parameter_names}}"
      )
    }

    # check list
    is_each_number <- all(
      vapply(
        parameters, checkmate::test_number, logical(1L),
        lower = 0.0, finite = TRUE
      )
    )

    if (!is_each_number) {
      cli::cli_abort(
        "Expected each parameters passed in `...` to be a single positive and
        finite number."
      )
    }
  }

  params <- daedalus::vaccination_scenario_data[[name]]
  params[names(parameters)] <- parameters

  x <- new_daedalus_vaccination(
    name, params
  )

  validate_daedalus_vaccination(x)

  x
}

#' Validator for the `<daedalus_vaccination>` class
#'
#' @param x An object to be validated as a `<daedalus_vaccination>` object.
#'
#' @keywords internal
#' @noRd
validate_daedalus_vaccination <- function(x) {
  if (!is_daedalus_vaccination(x)) {
    cli::cli_abort(
      "Object should be of class {.cls daedalus_vaccination};
      check class assignment."
    )
  }

  # check class members
  expected_invariants <- c(
    "name", daedalus::vaccination_parameter_names
  )
  has_invariants <- checkmate::test_names(
    attributes(x)$names,
    permutation.of = expected_invariants
  )
  if (!has_invariants) {
    cli::cli_abort(
      "`x` is class {.cls daedalus_vaccination} but does not have the correct
      attributes"
    )
  }

  stopifnot(
    "Vaccination `name` must be among `daedalus::vaccination_scenario_names`" =
      checkmate::test_string(x$name) &&
        checkmate::test_subset(
          x$name, daedalus::vaccination_scenario_names
        )
  )

  invisible(
    lapply(daedalus::vaccination_parameter_names, function(n) {
      lgl <- checkmate::test_number(x[[n]], lower = 0.0, finite = TRUE)
      if (!lgl) {
        cli::cli_abort(
          "<daedalus_vaccination> member {.str {n}} must be a single finite
          positive number"
        )
      }
    })
  )

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
#' @name class_vaccination
#' @param x An object of the `<daedalus_vaccination>` class.
#' @param ... Other parameters passed to [print()].
#' @export
print.daedalus_vaccination <- function(x, ...) {
  validate_daedalus_vaccination(x)
  format(x, ...)
}

#' Format a `<daedalus_vaccination>` object
#'
#' @param x A `<daedalus_vaccination>` object.
#' @param ... Other arguments passed to [format()].
#'
#' @return Invisibly returns the `<daedalus_vaccination>` object `x`.
#' Called for printing side-effects.
#' @keywords internal
#' @noRd
format.daedalus_vaccination <- function(x, ...) {
  chkDots(...)

  # NOTE: rough implementations, better scaling e.g. to millions could be added
  cli::cli_text("{.cls {class(x)}}")
  cli::cli_text(
    "Advance vaccine investment: {cli::style_bold(x$name)}"
  )
  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
  cli::cli_bullets(
    class = divid,
    c(
      "*" = "Start time (days): {.val {x$vax_start_time}}",
      "*" = "Rate (% per day): {.val {x$nu}}",
      "*" = "Uptake limit (%): {x$vax_uptake_limit}"
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
    checkmate::test_subset(to_get, names(x))

  if (!good_to_get) {
    cli::cli_abort(
      c(
        "`to_get` must be a string available in the {.cls class(x)}",
        i = "Allowed values are {.str {names(x)}}"
      )
    )
  }

  x[[to_get]]
}

#' @name set_data
#' @export
set_data.daedalus_vaccination <- function(x, ...) {
  to_set <- rlang::list2(...)
  checkmate::assert_list(to_set, "numeric", any.missing = FALSE)

  is_good_subs <- checkmate::test_subset(
    names(to_set), daedalus::vaccination_parameter_names
  )
  if (!is_good_subs) {
    cli::cli_abort(
      "Found a disallowed parameter substitution in `set_data()`: you are
      trying to set a vaccination parameter that is not supported by the
      DAEDALUS model."
    )
  }

  x[names(to_set)] <- to_set

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
  x <- unclass(x)

  # convert percentages to proportions
  x[["nu"]] <- x[["nu"]] / 100.0
  x[["uptake_limit"]] <- x[["uptake_limit"]] / 100.0

  x[names(x) != "name"]
}

#' Scale vaccination rate by remaining eligibles
#'
#' @param state The system state as a 4-dimensional array.
#' @param nu The vaccination rate.
#'
#' @return The scaled vaccination rate.
#' @keywords internal
scale_nu <- function(state, nu, uptake_limit) {
  total <- sum(state)
  eligible <- state[, c(i_S, i_R), , i_UNVACCINATED_STRATUM]

  total_vaccinated <- sum(state[, , , i_VACCINATED_STRATUM])
  prop_vaccinated <- total_vaccinated / total

  # NOTE: simplified scaling works only for uniform rates and start times
  # across age groups
  # NOTE: scale vaccination rate using a sigmoid function around the uptake
  # limit for a smoother transition
  scaling <- (total / sum(eligible)) *
    (1.0 / (1.0 + exp(prop_vaccinated - uptake_limit)))

  # handle conditions:
  # - scaling is finite: return minimum of 1.0 or scaled `nu`
  # prevent more vaccinations than eligibles
  # - scaling is inifite due to zero division (no eligibles): return 0
  if (is.finite(scaling)) {
    min(1.0, scaling * nu)
  } else {
    0.0
  }
}
