#' Helper functions
#'
#' @name tools
#' @rdname tools
#'
#' @keywords internal
drop_null <- function(x) {
  Filter(function(y) !is.null(y), x)
}

#' Check that a class has expected names
#'
#' @name input_checkers
#' @rdname input_checks
#'
#' @param x An object to be tested as having `expected_fields` class member
#' names.
#'
#' @param expected_fields A character vector of expected class member names.
#'
#' @return Invisibly returns `x`; primarily called for its side effects.
#'
#' @keywords internal
assert_class_fields <- function(x, expected_fields) {
  names_x <- attr(x, "names")
  has_fields <- checkmate::test_names(
    names_x,
    permutation.of = expected_fields
  )

  if (!has_fields) {
    missing_fields <- setdiff(names_x, expected_fields) # nolint used in err

    cli::cli_abort(
      "`x` is class {.cls {class(x)}} and has the extra or missing fields\
      {.str {missing_fields}}"
    )
  }

  invisible(x)
}
