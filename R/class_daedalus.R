#' Convert a list to `<daedalus>` output
#'
#' @name class_daedalus
#' @rdname class_daedalus
#'
#' @param x A `<list>` to convert to the `<daedalus>` output class.
#' @return
#' An object of the `<daedalus>` output class.
#' @keywords internal
as_daedalus <- function(x) {
  class(x) <- "daedalus"
  validate_daedalus(x)

  x
}

#' Validate a potential `<daedalus>` class object
#' @name class_daedalus
#' @param x An object to be validated as being of the `<daedalus>` class.
#' @return Invisibly returns `x`; called primarily for its side effects of
#' erroring when the object does not satisfy the `<daedalus>` class
#' requirements.
validate_daedalus <- function(x) {
  stopifnot(
    "Object should be of class `daedalus`" =
      is_daedalus(x)
  )

  invisible(x)
}

#' Check if an object is of the `<daedalus>` class
#' @name class_daedalus
#' @param x An object to be checked as inheriting from the `<daedalus>` class.
#' @return A logical for whether `x` is of the `<daedalus>` class.
is_daedalus <- function(x) {
  inherits(x, "daedalus")
}

#' Print `<daedalus>` class objects
#' @name class_daedalus
#' @param x An object of the `<daedalus>` class.
#' @return None; called for its printing side effects.
#' @export
print.daedalus <- function(x, ...) {
  format(x, ...)
}

#' @keywords internal
#' @noRd
format.daedalus <- function(x, ...) {
  chkDots(...)
  validate_daedalus(x)

  cli::cli_text("{.cls daedalus}")
  cli::cli_text("Model output and parameters with {nrow(x$data)} rows; head:")
  print(
    head(x[["data"]])
  )
  cli::cli_text(
    "Access timeseries as `x$data` and parameters as `x$parameters`"
  )

  invisible(x)
}
