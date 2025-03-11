#' Constructor for the `<daedalus_infection>` class
#'
#' @description Internal constructor function to make new objects of class
#' `<daedalus_infection>`. See [daedalus_infection()] for the user-facing helper
#' function which calls this function internally.
#'
#' @param name An epidemic name from among [daedalus::epidemic_names].
#' @param parameters A named list of parameters for the infection.
#'
#' @return An object of the `<daedalus_infection>` class, which inherits from a
#' `<list>`.
#' @keywords internal
#' @noRd
new_daedalus_infection <- function(name, parameters) {
  # all input checking at top level
  x <- c(list(name = name), parameters)
  class(x) <- "daedalus_infection"

  x
}

#' Represent infection parameters for DAEDALUS
#'
#' @name class_infection
#' @rdname class_infection
#'
#' @description Helper functions to create and work with S3 class
#' `<daedalus_infection>` objects for use with [daedalus()].
#' These objects store infection parameters for reuse and have methods for easy
#' parameter access and editing, as well as processing raw infection
#' characteristics for the DAEDALUS model.
#'
#' @param name An epidemic name from among [daedalus::epidemic_names].
#' Selecting an epidemic automatically pulls in infection parameters
#' associated with the epidemic; these are stored as packaged data in
#' `daedalus::infection_data`. Default infection parameters for epidemics can be
#' over-ridden by passing them as a named list to `...`.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Manually specified values for
#' any of the infection parameters. See **Details** for which infection
#' parameters are supported.
#'
#' @export
#' @return
#'
#' - `daedalus_infection()` returns an object of the S3 class
#' `<daedalus_infection>`.
#'
#' - `is_daedalus_infection()` returns a logical for whether an object is a
#' `<daedalus_infection>`.
#'
#' - `print.daedalus_infection()` invisibly returns the `<daedalus_infection>`
#' object `x`. Called for printing side-effects.
#'
#' @details
#'
#' ## Included epidemics
#'
#' Epidemics for which data are available are given below (pathogen in
#' parentheses). The string indicates the name that must be passed to the `name`
#' argument.
#'
#' - `"sars_cov_1"`: SARS 2004 (SARS-CoV-1),
#'
#' - `"influenza_2009"`: influenza 2009 (influenza A H1N1),
#'
#' - `"influenza_1957"`: influenza 1957 (influenza A H2N2),
#'
#' - `"influenza_1918"`: influenza 1918 (influenza A H1N1),
#'
#' - `"sars_cov_2_pre_alpha"`: Covid-19 wild type (SARS-Cov-2 wild type),
#'
#' - `"sars_cov_2_omicron"`: Covid-19 Omicron (SARS-CoV-2 omicron),
#'
#' - `"sars_cov_2_pre_delta"`: (SARS-CoV-2 delta).
#'
#' ## Infection parameters
#'
#' All infections have the following parameters, which take default values
#' stored in the package under [daedalus::infection_data]. Users can pass
#' custom values for these parameters as arguments via `...`.
#'
#' - `r0`: A single numeric value for the basic reproduction value of the
#' infection \eqn{R_0}.
#'
#' - `sigma`: A single numeric value > 0.0 for the rate of transition from the
#' exposed compartment to one of two infectious compartments.
#'
#' - `p_sigma`: A single numeric value in the range \eqn{[0.0, 1.0]} for the
#' proportion of infectious individuals who are also symptomatic. Asymptomatic
#' individuals can have a different contribution to the force of infection from
#' symptomatic individuals.
#'
#' - `epsilon`: A single numeric value for the relative contribution of
#' asymptomatic infectious individuals to the force of infection (compared to
#' symptomatic individuals).
#'
#' - `gamma_Is`: A single numeric value for the recovery rate of infectious
#' individuals who are not hospitalised.
#'
#' - `gamma_Ia`: A single numeric value for the recovery rate from asymptomatic
#' infection.
#'
#' - `ifr`: A numeric vector of length `N_AGE_GROUPS` (4) for the
#' age-specific infection fatality risk.
#'
#' - `gamma_H`: A numeric vector of length `N_AGE_GROUPS` (4) for the
#' age-specific recovery rate for individuals who are hospitalised.
#'
#' - `eta`: A numeric vector of length `N_AGE_GROUPS` (4) for the age-specific
#' hospitalisation rate for individuals who are infectious and symptomatic.
#'
#' - `omega`: A numeric vector of length `N_AGE_GROUPS` (4) for the age-specific
#' mortality rate for individuals who are hospitalised.
#'
#' - `rho`: A single numeric value for the rate at which infection-derived
#' immunity wanes, returning individuals in the 'recovered' compartment to the
#' 'susceptible' compartment.
#'
#' @examples
#' # make a <daedalus_infection> object with default parameter values
#' daedalus_infection("sars_cov_1")
#'
#' # modify infection parameters R0 and immunity waning rate
#' daedalus_infection("influenza_1918", r0 = 2.5, rho = 0.01)
daedalus_infection <- function(name, ...) {
  # input checking
  name <- rlang::arg_match(name, daedalus::epidemic_names)
  parameters <- rlang::list2(...)

  is_empty_list <- checkmate::test_list(parameters, len = 0)

  if (!is_empty_list) {
    # check list
    checkmate::assert_list(
      parameters,
      "numeric",
      min.len = 0,
      max.len = length(daedalus::infection_parameter_names)
    )

    has_good_names <- checkmate::test_subset(
      names(parameters),
      daedalus::infection_parameter_names,
      empty.ok = TRUE
    )
    if (!has_good_names) {
      cli::cli_abort(
        "Found unexpected values in `...`; the only allowed parameters are:
        {.str {daedalus::infection_parameter_names}}"
      )
    }

    allowed_numerics_names <- c("ifr", "eta", "gamma_H", "omega")
    is_each_number <- all(vapply(
      parameters[!names(parameters) %in% allowed_numerics_names],
      checkmate::test_number,
      logical(1L),
      # NOTE: rate parameter limits allowed may need to be tweaked
      lower = 0.0,
      finite = TRUE
    ))

    is_numeric_good <- all(vapply(
      parameters[allowed_numerics_names],
      checkmate::test_numeric,
      logical(1L),
      len = N_AGE_GROUPS,
      lower = 0.0,
      finite = TRUE,
      null.ok = TRUE
    ))

    if (!is_each_number) {
      cli::cli_abort(c(
        "Expected the following parameters passed in `...`
          to be a single positive and finite number:
          {.str {
            setdiff(daedalus::infection_parameter_names,
            allowed_numerics_names)
          }}",
        i = "Only {.str {allowed_numerics_names}} may be numeric vectors.
          See the Help page for {.help daedalus::daedalus} for parameters that
          can be numerics."
      ))
    }

    if (!is_numeric_good) {
      cli::cli_abort(c(
        "Expected the following parameters passed in `infect_params_manual`
          to be numeric vectors of length {N_AGE_GROUPS} with positive and
          finite values:
          {.str {intersect(names(parameters), allowed_numerics_names)}}",
        i = "See the Help page for {.help daedalus::daedalus} for parameters
          that can be numerics."
      ))
    }
  }

  # substitute defaults with non-NULL elements of parameters
  params <- daedalus::infection_data[[name]]
  params[names(parameters)] <- parameters

  x <- new_daedalus_infection(name, params)

  validate_daedalus_infection(x)

  x
}

#' Validator for the `<daedalus_infection>` class
#'
#' @param x An object to be validated as a `<daedalus_infection>` object.
#'
#' @keywords internal
#' @noRd
validate_daedalus_infection <- function(x) {
  if (!is_daedalus_infection(x)) {
    cli::cli_abort(
      "Object should be of class {.cls daedalus_infection};
      check class assignment."
    )
  }

  # check class members
  expected_invariants <- c("name", daedalus::infection_parameter_names)
  has_invariants <- checkmate::test_names(
    attributes(x)$names,
    must.include = expected_invariants
  )
  if (!has_invariants) {
    cli::cli_abort(
      "`x` is class {.cls daedalus_infection} but does not have the correct
      attributes"
    )
  }

  # check class members
  allowed_numerics_names <- c("ifr", "eta", "gamma_H", "omega")
  expected_number <- setdiff(
    daedalus::infection_parameter_names,
    allowed_numerics_names
  )

  # fmt: skip
  stopifnot(
    "Infection `name` must be a string from `daedalus::epidemic_names`" =
      checkmate::test_string(x$name) &&
        checkmate::test_subset(
          x$name, daedalus::epidemic_names
        )
  )
  invisible(lapply(expected_number, function(n) {
    lgl <- checkmate::test_number(x[[n]], lower = 0.0, finite = TRUE)
    if (!lgl) {
      cli::cli_abort(
        "<daedalus_infection> member {.str {n}} must be a single finite
          positive number"
      )
    }
  }))
  invisible(lapply(allowed_numerics_names, function(n) {
    lgl <- checkmate::test_numeric(
      x[[n]],
      lower = 0.0,
      finite = TRUE,
      len = N_AGE_GROUPS
    )
    if (!lgl) {
      cli::cli_abort(
        "<daedalus_infection> member {.str {n}} must be a numeric vector of
          length 4 (number of age groups)"
      )
    }
  }))

  invisible(x)
}

#' Check if an object is a `<daedalus_infection>`
#' @name class_infection
#'
#' @export
is_daedalus_infection <- function(x) {
  inherits(x, "daedalus_infection")
}

#' Print a `<daedalus_infection>` object
#' @name class_infection
#' @param x An object of the `<daedalus_infection>` class.
#' @param ... Other parameters passed to [print()].
#' @export
print.daedalus_infection <- function(x, ...) {
  validate_daedalus_infection(x)
  format(x, ...)
}

#' Format a `<daedalus_infection>` object
#'
#' @param x A `<daedalus_infection>` object.
#' @param ... Other arguments passed to [format()].
#'
#' @return Invisibly returns the `<daedalus_infection>` object `x`.
#' Called for printing side-effects.
#' @keywords internal
#' @noRd
format.daedalus_infection <- function(x, ...) {
  chkDots(...)

  # NOTE: not showing IFR as this is also a function of country demography
  cli::cli_text("{.cls {class(x)}}")
  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
  cli::cli_bullets(
    class = divid,
    c(
      "*" = "Epidemic name: {cli::style_bold(x$name)}",
      "*" = "R0: {cli::col_red(x$r0)}",
      "*" = "sigma: {.val {x$sigma}}",
      "*" = "p_sigma: {.val {x$p_sigma}}",
      "*" = "epsilon: {.val {x$epsilon}}",
      "*" = "rho: {.val {x$rho}}",
      "*" = "eta: {.val {x$eta}}",
      "*" = "omega: {.val {x$omega}}",
      "*" = "gamma_Ia: {.val {x$gamma_Ia}}",
      "*" = "gamma_Is: {.val {x$gamma_Is}}",
      "*" = "gamma_H: {.val {x$gamma_H}}"
    )
  )
  cli::cli_end(divid)

  invisible(x)
}

#' @name get_data
#' @export
get_data.daedalus_infection <- function(x, to_get, ...) {
  chkDots(...)
  validate_daedalus_infection(x)

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
set_data.daedalus_infection <- function(x, ...) {
  to_set <- rlang::list2(...)
  checkmate::assert_list(to_set, "numeric", any.missing = FALSE)

  is_good_subs <- checkmate::test_subset(
    names(to_set),
    daedalus::infection_parameter_names
  )
  if (!is_good_subs) {
    cli::cli_abort(
      "Found a disallowed parameter substitution in `set_data()`: you are
      trying to set an infection parameter that is not supported by the
      DAEDALUS model."
    )
  }

  x[names(to_set)] <- to_set

  validate_daedalus_infection(x)

  x
}

#' Prepare infection parameters for model
#'
#' @name prepare_parameters
#'
#' @keywords internal
prepare_parameters.daedalus_infection <- function(x, ...) {
  chkDots(...)

  validate_daedalus_infection(x)
  x <- unclass(x)

  age_varying_params <- c("eta", "omega", "gamma_H")

  x[age_varying_params] <- lapply(x[age_varying_params], function(p) {
    p <- c(p, rep(p[i_WORKING_AGE], N_ECON_SECTORS))
    p
  })

  x[names(x) != "name"]
}
