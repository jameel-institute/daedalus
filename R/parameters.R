#' @title Default parameters for DAEDALUS
#' @description Useful defaults for the DAEDALUS model for use during
#' development.
#' @param ... User-specified values to replace default parameter values.
#' @export
#' @examples
#' default_parameters(beta = 1.5 / 7.0)
default_parameters <- function(...) {
  user_params <- list(...)

  contact_matrix <- matrix(1.0, N_AGE_GROUPS, N_AGE_GROUPS)
  # lower for not in work
  cmw <- c(0.0, rep(10.0, N_ECON_SECTORS - 1L))

  # NOTE: default assumption is uniformly distributed contacts
  cmcw <- matrix(
    rep(
      rep(1.0, N_AGE_GROUPS),
      N_ECON_SECTORS
    ),
    nrow = N_ECON_SECTORS,
    ncol = N_AGE_GROUPS
  )

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
    rho = 1.0 / 180.0,
    contact_matrix = contact_matrix,
    contacts_workplace = cmw,
    contacts_consumer_worker = cmcw
  )

  is_empty_list <- checkmate::test_list(user_params, null.ok = TRUE, len = 0L)
  if (!is_empty_list) {
    is_each_number <- all(
      vapply(
        X = user_params[!grepl("contact", names(user_params), fixed = TRUE)],
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
      is_good_cm <- checkmate::test_matrix(
        user_params[["contact_matrix"]],
        mode = "numeric",
        any.missing = FALSE
      )
      if (!is_good_cm) {
        cli::cli_abort(
          "Expected user-provided `contact_matrix` to have no missing elements."
        )
      }

      is_good_cm_dims <- checkmate::test_matrix(
        user_params[["contact_matrix"]],
        nrows = N_AGE_GROUPS, ncols = N_AGE_GROUPS
      )
      if (!is_good_cm_dims) {
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

    if ("contacts_workplace" %in% names(user_params)) {
      is_good_cw <- checkmate::test_numeric(
        user_params[["contacts_workplace"]],
        len = N_ECON_SECTORS,
        lower = 0.0, finite = TRUE, any.missing = FALSE
      )
      if (!is_good_cw) {
        cli::cli_abort(
          c(
            "Expected user-provided `contacts_workplace` to be a numeric vector
            with {N_ECON_SECTORS} finite elements >= 0.0, but it had
            {length(user_params[['contacts_workplace']])} elements",
            i = "The length of the vector corresponds to the number of economic
            sectors in the model."
          )
        )
      }
    }

    if ("contacts_consumer_worker" %in% names(user_params)) {
      is_good_cmcw <- checkmate::test_matrix(
        user_params[["contacts_consumer_worker"]],
        mode = "numeric",
        any.missing = FALSE
      ) && checkmate::test_numeric(
        user_params[["contacts_consumer_worker"]],
        lower = 0.0,
        finite = TRUE
      )
      is_good_cmcw_dims <- checkmate::test_matrix(
        user_params[["contacts_consumer_worker"]],
        nrows = N_ECON_SECTORS, ncols = N_AGE_GROUPS
      )

      if (!is_good_cmcw) {
        cli::cli_abort(
          "Expected user-provided `contacts_consumer_worker` to be a numeric
          matrix with no missing elements and with all elements finite and >=
          0.0."
        )
      }
      if (!is_good_cmcw_dims) {
        cli::cli_abort(
          c(
            "Expected user-provided `contacts_consumer_worker` to have
            {N_ECON_SECTORS} rows and {N_AGE_GROUPS} columns, but it has
            {nrow(user_params[['contacts_consumer_worker']])} rows and
            {ncol(user_params[['contacts_consumer_worker']])} columns.",
            i = "The number of rows corresponds to the number of economic
            sectors, and the number of colums to the number of demographic
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
