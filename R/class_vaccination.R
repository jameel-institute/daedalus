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
  x <- c(list(name = name), parameters)

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
#' `daedalus::vaccination_scenario_data`.
#'
#' @param start_time The number of days after the start of the epidemic that
#' vaccination begins. Must be a single number. Defaults to `NULL` and the start
#' time is taken from the vaccination scenarios specified by `name`.
#'
#' @param rate A single number for the _percentage_ of the total population that
#' can be vaccinated each day. This is converted into a proportion automatically
#' within [daedalus()].
#'
#' @param uptake_limit A single number giving the upper limit for the
#' _percentage_ of the population that can be vaccinated. When this limit is
#' reached, vaccination ends.
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
#' @param ... For the `print` method, other parameters passed to [print()].
#'
#' @details
#' Note that vaccination once ended by reaching the `uptake_limit` does not
#' restart once individuals wane out of the vaccinated compartment.
#'
#' @export
#' @examples
#' # for no advance vaccine investment
#' daedalus_vaccination("none")
#'
#' # modifying parameters during initialisation
#' # set daily vaccination rate to 1.5% of population
#' daedalus_vaccination("low", rate = 1.5)
daedalus_vaccination <- function(
  name,
  start_time = NULL,
  rate = NULL,
  uptake_limit = NULL,
  efficacy = 50,
  waning_period = 180
) {
  # input checking
  name <- rlang::arg_match(name, daedalus::vaccination_scenario_names)

  checkmate::assert_integerish(start_time, lower = 0, null.ok = TRUE)
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

  params <- daedalus::vaccination_scenario_data[[name]]
  params[names(user_params)] <- user_params

  x <- new_daedalus_vaccination(name, params)

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
  expected_invariants <- c("name", daedalus::vaccination_parameter_names)
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

  # fmt: skip
  stopifnot(
    "Vaccination `name` must be among `daedalus::vaccination_scenario_names`" =
      checkmate::test_string(x$name) &&
        checkmate::test_subset(
          x$name, daedalus::vaccination_scenario_names
        )
  )

  invisible(lapply(daedalus::vaccination_parameter_names, function(n) {
    lgl <- checkmate::test_number(x[[n]], lower = 0.0, finite = TRUE)
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
#' @name class_vaccination
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

  cli::cli_text("{.cls {class(x)}}")
  cli::cli_text("Advance vaccine investment: {cli::style_bold(x$name)}")
  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
  cli::cli_bullets(
    class = divid,
    c(
      "*" = "Start time (days): {.val {x$start_time}}",
      "*" = "Rate (% per day): {.val {x$rate}}",
      "*" = "Uptake limit (%): {x$uptake_limit}",
      "*" = "Efficacy (%): {x$efficacy * 100}",
      "*" = "Waning period (mean, days): {x$waning_period}"
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
    cli::cli_abort(c(
      "`to_get` must be a string available in the {.cls class(x)}",
      i = "Allowed values are {.str {names(x)}}"
    ))
  }

  x[[to_get]]
}

#' @name set_data
#' @export
set_data.daedalus_vaccination <- function(x, ...) {
  to_set <- rlang::list2(...)
  checkmate::assert_list(to_set, "numeric", any.missing = FALSE)

  is_good_subs <- checkmate::test_subset(
    names(to_set),
    daedalus::vaccination_parameter_names
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

  # convert percentages to proportions, and times to rates
  x[["nu"]] <- x[["rate"]] / 100.0
  x[["vax_uptake_limit"]] <- x[["uptake_limit"]] / 100.0
  x[["tau"]] <- c(1, x[["efficacy"]] / 100.0)
  x[["psi"]] <- 1 / x[["waning_period"]]
  x[["vax_start_time"]] <- x[["start_time"]]

  x[
    !names(x) %in%
      c("name", "rate", "waning_period", "start_time", "uptake_limit")
  ]
}

#' Scale vaccination rate by remaining eligibles
#'
#' @param state The system state as a 4-dimensional array.
#' @param nu The vaccination rate.
#'
#' @return The scaled vaccination rate.
#' @keywords internal
scale_nu <- function(state, nu, uptake_limit) {
  # NOTE: state must be a 4D array with only vaccinated and unvaccinated layers
  # in dim 4
  total <- sum(state[,
    i_EPI_COMPARTMENTS,
    c(i_VACCINATED_STRATUM, i_UNVACCINATED_STRATUM)
  ])
  total_vaccinated <- sum(state[, i_EPI_COMPARTMENTS, i_VACCINATED_STRATUM])
  prop_vaccinated <- total_vaccinated / total

  # NOTE: simplified scaling works only for uniform rates and start times
  # across age groups
  # NOTE: scale vaccination rate using a sigmoid function around the uptake
  # limit for a smoother transition
  scaled_nu <- (nu / (1 - prop_vaccinated)) /
    (1.0 + exp(100.0 * (prop_vaccinated - uptake_limit)))

  # handle conditions:
  # - scaling is finite: return minimum of 1.0 or scaled `nu`
  # prevent more vaccinations than eligibles
  # - scaling is inifite due to zero division (no eligibles): return 0
  if (is.finite(scaled_nu)) {
    min(1.0, scaled_nu)
  } else {
    0.0
  }
}

#' Replace prepare_parameters() for vaccinations
#'
#' @name prepare_parameters
#'
#' @keywords internal
prepare_parameters2.daedalus_vaccination <- function(x, ...) {
  chkDots(...)
  validate_daedalus_vaccination(x)

  # only need rates for nu and psi for now
  list(
    nu = get_data(x, "rate") / 100,
    psi = 1 / get_data(x, "waning_period"),
    uptake_limit = get_data(x, "uptake_limit") / 100
  )
}

#' Dummy vaccination
#'
#' @return A `daedalus_vaccination` object intended to have no effect;
#' vaccination rate and efficacy are set to zero.
#'
#' @keywords internal
dummy_vaccination <- function() {
  daedalus_vaccination("none", rate = 0, efficacy = 0)
}
