#' Helper functions
#'
#' @name tools
#' @rdname tools
#'
#' @details
#' `weighted_rowsums()` is used to get the row-wise sums of a matrix after
#' column-wise multiplication by a vector of weights.
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

#' @name tools
#' @keywords internal
first <- function(x) {
  x[[1L]]
}

#' @name tools
#' @keywords internal
last <- function(x) {
  x[[length(x)]]
}

#' @name tools
#' @keywords internal
weighted_rowsums <- function(x, weights) {
  checkmate::assert_matrix(x, "numeric", any.missing = FALSE)
  checkmate::assert_numeric(
    weights,
    len = ncol(x)
  )

  rowSums(x %*% diag(weights))
}

#' @name tools
#' @keywords internal
interest_accumulation <- function(principal, contribution, rate) {
  contribution + (1.0 + rate) * principal
}
