#' Convert a list to `<daedalus_output>`
#'
#' @name class_daedalus_output
#' @rdname class_daedalus_output
#'
#' @param x A `<list>` to convert to the `<daedalus_output>` class.
#' @return
#' An object of the `<daedalus_output>` class.
#' @keywords internal
as_daedalus_output <- function(x) {
  checkmate::assert_list(x, c("data.frame", "NULL"))
  class(x) <- "daedalus_output"
  validate_daedalus_output(x)

  x
}

#' Validate a potential `<daedalus_output>` class object
#' @name class_daedalus_output
#' @param x An object to be validated as being of the `<daedalus_output>` class.
#' @return Invisibly returns `x`; called primarily for its side effects of
#' erroring when the object does not satisfy the `<daedalus_output>` class
#' requirements.
#' @keywords internal
validate_daedalus_output <- function(x) {
  expected_invariants <- c(
    "model_data",
    # NOTE: reserving 'parameters' for values fixed before model run
    "country_parameters", "infection_parameters",
    "response_data" # includes response strategy and implementation level
  )

  stopifnot(
    "Object should be of class `daedalus_output`" =
      is_daedalus_output(x),
    "Object does not have expected members" =
      checkmate::test_names(
        names(x),
        must.include = expected_invariants
      )
  )

  invisible(x)
}

#' Check if an object is of the `<daedalus_output>` class
#' @name class_daedalus_output
#' @param x An object to be checked as inheriting from the `<daedalus_output>`
#' class.
#' @return A logical for whether `x` is of the `<daedalus_output>` class.
#' @keywords internal
is_daedalus_output <- function(x) {
  inherits(x, "daedalus_output")
}

#' Print `<daedalus_output>` class objects
#' @name class_daedalus_output
#' @param x An object of the `<daedalus_output>` class.
#' @return None; called for its printing side effects.
#' @export
print.daedalus_output <- function(x, ...) {
  format(x, ...)
}

#' @keywords internal
#' @noRd
format.daedalus_output <- function(x, ...) {
  chkDots(...)
  validate_daedalus_output(x)

  cli::cli_text("{.cls daedalus_output}")
  cli::cli_text("Model output and parameters with {nrow(x$data)} rows; head:")
  print(
    head(x[["model_data"]])
  )
  cli::cli_bullets(
    c(
      "*" = "Access model timeseries as `x$model_data`",
      "*" = "Access country parameters as `x$country_parameters`",
      "*" = "Access infection parameters as `x$infection_parameters`",
      "*" = "Access response strategy information as `x$response_data`"
    )
  )

  invisible(x)
}
