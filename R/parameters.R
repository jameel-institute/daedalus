#' @title Make a parameter list for DAEDALUS
#' @description Helper function to prepare a parameter list for `daedalus()`,
#' allowing the use of country-specific contact matrices and user-specified
#' infection parameter values.
#' @inheritParams daedalus
#' @param ... User-specified values to replace default infection or country
#' parameter values.
#' @return A list of model parameters suitable for the country or territory
#' specified in `country` and for the infection specified in `epidemic`, with
#' any user-specified over-rides from `...`.
#' @keywords internal
make_parameters <- function(country, epidemic, ...) {
  user_params <- list(...)

  # NOTE: country name is input checked in top-level fn `daedalus()`
  country_params <- daedalus::country_data[[country]]
  demography <- country_params[["demography"]]
  economic_contacts <- daedalus::economic_contacts

  # NOTE: scaling by leading eigenvalue needed to parameterise transmission
  # using r0 - scaling could be moved to data prep step if data are not to be
  # served to users and are purely internal
  cm <- country_params[["contact_matrix"]]
  cm <- (cm / max(Re(eigen(cm, only.values = TRUE)$value))) / demography

  # workplace contacts within sectors
  cmw <- economic_contacts[["contacts_workplace"]]
  cmw <- cmw / max(Re(eigen(diag(cmw))$values))

  # NOTE: default assumption for consumer to worker contacts
  # is workplace contacts `cmw` distributed in proportion to demography
  cmcw <- matrix(
    cmw, N_ECON_SECTORS, N_AGE_GROUPS
  ) * demography / sum(demography)
  # NOTE: scaling by largest singular value using base::svd, accessed as "d"
  cmcw <- cmcw / max(Re(svd(cmcw)[["d"]]))

  # NOTE: dummy CM for between-sector contacts; see data-raw/economic_contacts.R
  cm_ww <- economic_contacts[["contacts_between_sectors"]]

  # get infection params from `infection_data`
  infection_params <- daedalus::infection_data[[epidemic]]
  country_params <- list(
    demography = demography,
    contact_matrix = cm,
    contacts_workplace = cmw,
    contacts_consumer_worker = cmcw,
    contacts_between_sectors = cm_ww
  )

  params <- c(infection_params, country_params)

  # check all user inputs
  is_empty_list <- checkmate::test_list(user_params, null.ok = TRUE, len = 0L)
  if (!is_empty_list) {
    # NOTE: some infection parameters must be numbers (numeric length 1)
    # while others are numeric vectors of length `N_AGE_GROUPS`
    allowed_numerics_names <- c("eta", "gamma_H", "omega")
    contact_data_names <- c(
      "contact_matrix", "contacts_workplace", "contacts_consumer_worker",
      "contacts_between_sectors"
    )
    is_each_number <- all(
      vapply(
        user_params[!names(user_params) %in%
          c(allowed_numerics_names, contact_data_names)],
        checkmate::test_number,
        logical(1L),
        # NOTE: rate parameter limits allowed may need to be tweaked
        lower = 0.0,
        finite = TRUE
      )
    )

    is_numeric_good <- all(
      vapply(
        user_params[allowed_numerics_names], checkmate::test_numeric,
        logical(1L),
        len = N_AGE_GROUPS, lower = 0.0,
        finite = TRUE, null.ok = TRUE
      )
    )

    if (!is_each_number) {
      cli::cli_abort(
        c(
          "Expected the following parameters passed in `...` to be a single
          positive and finite number:
          {.str {setdiff(
            names(user_params), c(allowed_numerics_names, contact_data_names)
          )}}",
          i = "Only user-provided contact data may be numeric vectors
          or matrices. See the Help page for {.help daedalus::daedalus} for
          parameters that can be user specified."
        )
      )
    }

    if (!is_numeric_good) {
      cli::cli_abort(
        c(
          "Expected the following parameters passed in `...` to be numeric
          vectors of length {N_AGE_GROUPS} with positive and finite values:
          {.str {intersect(names(user_params), allowed_numerics)}}",
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
            "Expected `contacts_between_sectors` to be a square
            numeric matrix with {N_ECON_SECTORS} rows and columns.",
            i = "The number of rows and columns corresponds to the number of
            economic sectors."
          ),
          .envir = parent.frame()
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
          "User provided parameters that are not among the model parameters;
          these will not be passed to the model:
          {.str {setdiff(names(user_params), names(params))}}.",
          i = "Model parameters are named: {.str {names(params)}}."
        ),
        call = parent.frame() # to show parent env rather than helper fn
      )
    }

    user_params[!names(user_params) %in% names(params)] <- NULL

    # replace defaults with user values
    params[names(user_params)] <- user_params
  }

  # return parameters
  params
}
