#' @title Default parameters for DAEDALUS
#' @param ... User-specified values to replace default parameter values.
#' @export
#' @examples
#' default_parameters(beta = 1.5 / 7.0)
default_parameters <- function(...) {
  user_params <- list(...)

  # NOTE: these are arbitrary values roughly equivalent to
  # pandemic influenza, with R0 = 1.3, infectious period = 7 days
  # pre-infectious period = 3 days
  params <- list(
    beta = 1.3 / 7.0,
    sigma = 1.0 / 3.0,
    p_sigma = 0.66,
    epsilon = 0.2,
    gamma = 1.0 / 7.0,
    eta = 1.0 / 100.0,
    omega = 1.0 / 100.0,
    contact_matrix = matrix(1.0, 4L, 4L)
  )

  is_empty_list <- checkmate::test_list(user_params, null.ok = TRUE, len = 0L)
  if (!is_empty_list) {
    is_each_number <- all(
      vapply(
        X = user_params[names(user_params) != "contact_matrix"],
        FUN = checkmate::test_number,
        FUN.VALUE = logical(1L),
        # NOTE: rate parameter limits allowed may need to be tweaked
        lower = 0.0,
        finite = TRUE
      )
    )
    if (!is_each_number) {
      cli::cli_abort(
        "Expected each user-provided parameter to be a single positive and
        finite number. Only user-provided `contact_matrix` may be a numeric
        matrix."
      )
    }

    if ("contact_matrix" %in% names(user_params)) {
      is_good_matrix <- checkmate::test_matrix(
        user_params[["contact_matrix"]],
        mode = "numeric",
        any.missing = FALSE
      )
      if (!is_good_matrix) {
        cli::cli_abort(
          "Expected user-provided `contact_matrix` to have no missing elements."
        )
      }

      is_good_matrix_dims <- checkmate::test_matrix(
        user_params[["contact_matrix"]],
        nrows = N_AGE_GROUPS, ncols = N_AGE_GROUPS
      )
      if (!is_good_matrix_dims) {
        cli::cli_abort(
          c(
            "Expected user-provided `contact_matrix` to be a square matrix with
            {N_AGE_GROUPS} rows and columns, but it had
            {nrow(user_params[['contact_matrix']])} rows and
            {nrow(user_params[['contact_matrix']])} columns.",
            i = "Number of rows and columns correspond to the number of age
            groups."
          )
        )
      }
    }

    # check for user values that cannot be used and remove
    has_identical_names <- checkmate::test_names(
      names(user_params),
      subset.of = c(names(params), NULL)
    )
    if (!has_identical_names) {
      cli::cli_warn(
        c(
          "The following user-provided parameters were passed that are not
          among the model parameters, and will not be passed to the model:
          {.str {setdiff(names(user_params), names(params))}}.",
          i = "Model parameters are named: {.str {names(params)}}."
        )
      )
    }
    user_params[!names(user_params) %in% names(params)] <- NULL

    # replace defaults with user values
    params[names(user_params)] <- user_params
  }

  # return parameters
  params
}
