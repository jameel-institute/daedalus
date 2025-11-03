#' Helper functions
#'
#' @name tools
#' @rdname tools
#'
#' @description
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
#'
#' @description
#' `interest_accumulation()` is used to calculate the new principal after
#' assuming an interest `rate` and with some external `contribution`. Used in
#' `get_fiscal_costs()` when the borrowed principal to fund pandemic response
#' both compounds over time, and increases due to fresh borrowing in each
#' timestep.
#'
#' @keywords internal
interest_accumulation <- function(principal, contribution, rate) {
  contribution + (1.0 + rate) * principal
}

#' @name tools
#'
#' @description
#' `annual_rate_daily()` is used to convert an annual rate of interest to a
#' daily rate, for use in `interest_accumulation()` and eventually in
#' `get_fiscal_costs()`.
#'
#' @keywords internal
annual_rate_daily <- function(x) {
  year_days <- 365.0

  # rate x assumed to be a decimal; e.g. 5% as 0.05
  (1 + x)^(1 / year_days) - 1
}
