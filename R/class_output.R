#' Create and work with `<daedalus_output>` objects
#'
#' @name class_daedalus_output
#' @rdname class_daedalus_output
#'
#' @param x A `<list>` to convert to the `<daedalus_output>` class.
#'
#' @return
#' An object of the `<daedalus_output>` class. Holds the raw ODE solution and
#' ODE events logs in \pkg{dust2} forma), as well as `country`, `infection`,
#' `vaccination`, and `behaviour` classes representing input arguments to
#' [daedalus()].
as_daedalus_output <- function(x) {
  checkmate::assert_list(
    x,
    c("data.frame", "list", "numeric", "NULL"),
    any.missing = TRUE # event data may be missing
  )
  class(x) <- "daedalus_output"
  validate_daedalus_output(x)

  x
}

#' @name class_daedalus_output
#'
#' @param x An object to be validated as being of the `<daedalus_output>` class.
#'
#' @return Invisibly returns `x`; called primarily for its side effects of
#' erroring when the object does not satisfy the `<daedalus_output>` class
#' requirements.
validate_daedalus_output <- function(x) {
  expected_fields <- c(
    "ode_soln",
    "ode_events",
    # NOTE: reserving 'parameters' for values fixed before model run
    "country",
    "infection",
    "vaccination",
    "behaviour",
    "response_data" # includes response strategy
  )

  stopifnot(
    "Object should be of class `daedalus_output`" = is_daedalus_output(x),
    "Object does not have expected members" = checkmate::test_names(
      names(x),
      must.include = expected_fields
    )
  )

  invisible(x)
}

#' @name class_daedalus_output
#'
#' @param x An object to be checked as inheriting from the `<daedalus_output>`
#' class.
#'
#' @return A logical for whether `x` is of the `<daedalus_output>` class.
is_daedalus_output <- function(x) {
  inherits(x, "daedalus_output")
}

#' Print `<daedalus_output>` class objects
#' @name class_daedalus_output
#'
#' @param x An object of the `<daedalus_output>` class.
#'
#' @param ... Not used; added for compatibility with the generic.
#'
#' @return None; called for its printing side effects.
#'
#' @export
print.daedalus_output <- function(x, ...) {
  format(x, ...)
}

#' @noRd
format.daedalus_output <- function(x, ...) {
  chkDots(...)
  validate_daedalus_output(x)

  # NOTE: placeholder formatting
  cli::cli_text("{.cls daedalus_output}")
  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
  cli::cli_bullets(
    class = divid,
    c(
      "*" = "Country: {cli::style_bold(x$country$name)}",
      "*" = "Epidemic: {cli::style_bold(x$infection$name)}",
      "*" = "NPI response:
        {cli::col_magenta(x$response_data$response_strategy)}",
      "*" = "Vaccination:
        {cli::col_magenta(x$vaccination$identifier)}",
      "*" = "Behaviour: {cli::col_magenta(x$behaviour$identifier)}"
    )
  )
  cli::cli_end(divid)

  invisible(x)
}

#' @name get_data
#'
#' @export
get_data.daedalus_output <- function(x, to_get = NULL, ...) {
  chkDots(...)
  validate_daedalus_output(x)

  good_to_get <- checkmate::test_string(to_get, null.ok = TRUE) &&
    checkmate::test_subset(to_get, c(names(x), "rt_data"))

  if (!good_to_get) {
    cli::cli_abort(c(
      "`to_get` must be a single string naming an element of `x`.",
      i = "Allowed values are {.str {names(x)}}"
    ))
  }

  # Return model timeseries on get_data(x) to reduce friction to use
  if (is.null(to_get)) {
    prepare_output(x[["ode_soln"]], x[["country"]], seq(0, x[["total_time"]]))
  } else if (to_get == "rt_data") {
    x[["ode_soln"]][["ipr"]]
  } else {
    x[[to_get]]
  }
}
