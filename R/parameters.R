#' @title Make a parameter list for DAEDALUS
#' @description Helper function to prepare a parameter list for `daedalus()`,
#' allowing the use of country-specific contact matrices and user-specified
#' pathogen parameter values.
#' @param country A string giving a country name for which stored demographic
#' and economic data are extracted.
#' @param ... User-specified values to replace default pathogen parameter values
#' .
make_parameters <- function(country, ...) {
  user_params <- list(...)

  # NOTE: country name is input checked in top-level fn `daedalus()`
  country_data_ <- daedalus::country_data[[country]]
  economic_contacts <- economic_contacts
  demography <- country_data_[["demography"]]

  cm <- country_data_[["contact_matrix"]] / demography

  # workplace contacts within sectors
  cmw <- economic_contacts[["contacts_workplace"]]

  # NOTE: default assumption for consumer to worker contacts
  # is workplace contacts `cmw` distributed in proportion to demography
  cmcw <- matrix(
    cmw, N_ECON_SECTORS, N_AGE_GROUPS
  ) * demography / sum(demography)

  # NOTE: dummy CM for between-sector contacts; see data-raw/economic_contacts.R
  cm_ww <- economic_contacts[["contacts_between_sectors"]]

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
    demography = demography,
    contact_matrix = cm,
    contacts_workplace = cmw,
    contacts_consumer_worker = cmcw,
    contacts_between_sectors = cm_ww
  )

  is_empty_list <- checkmate::test_list(user_params, null.ok = TRUE, len = 0L)
  if (!is_empty_list) {
    is_each_number <- all(
      vapply(
        user_params[!startsWith(names(user_params), "contact")],
        checkmate::test_number,
        logical(1L),
        # NOTE: rate parameter limits allowed may need to be tweaked
        lower = 0.0,
        finite = TRUE
      )
    )
    if (!is_each_number) {
      cli::cli_abort(
        c(
          "Expected each parameter passed in `...` to be a single positive and
          finite number. Only user-provided contact data may be numeric vectors
          or matrices.",
          i = "See the Help page for {.help daedalus::daedalus} for parameters
          that can be user specified."
        )
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

    if ("contacts_between_sectors" %in% names(user_params)) {
      is_good_cm_ww <- checkmate::test_matrix(
        user_params[["contacts_between_sectors"]],
        mode = "numeric", any.missing = FALSE
      ) && checkmate::test_numeric(
        user_params[["contacts_between_sectors"]],
        lower = 0.0, finite = TRUE
      )

      is_good_cm_ww_dims <- checkmate::test_matrix(
        user_params[["contacts_between_sectors"]],
        nrows = N_ECON_SECTORS, ncols = N_ECON_SECTORS
      )

      is_zero_diagonal <- all(
        diag(user_params[["contacts_between_sectors"]]) == 0.0
      )

      if (!is_good_cm_ww) {
        cli::cli_abort(
          "Expected user-provided `contacts_between_sectors` to be a numeric
          matrix with no missing elements and with all elements finite and >=
          0.0."
        )
      }
      if (!is_good_cm_ww_dims) {
        cli::cli_abort(
          c(
            "Expected user-provided `contacts_between_sectors` to be a square
            numeric matrix with {N_ECON_SECTORS} rows and columns, but it has
            {nrow(user_params[['contacts_consumer_worker']])} rows and
            {ncol(user_params[['contacts_consumer_worker']])} columns.",
            i = "The number of rows and columns corresponds to the number of
            economic sectors."
          )
        )
      }
      if (!is_zero_diagonal) {
        cli::cli_abort(
          c(
            "Expected user-provided `contacts_between_sectors` to be a hollow
            matrix whose diagonal entries are all zero.",
            i = "The diagonal represents worker-to-worker contacts within
            economic sectors, and should be passed as `contacts_workplace`."
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
