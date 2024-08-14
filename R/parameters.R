#' @title Make a list of model parameters for DAEDALUS
#'
#' @description Functions to access and prepare country demography, economic
#' sector, and contact data, and epidemic-specific infection parameter data,
#' while allowing users to pass custom values for contact data and infection
#' parameters.
#'
#' @name make_model_parameters
#' @rdname make_model_parameters
#'
#' @description Helper function to prepare a parameter list for `daedalus()`,
#' allowing the use of country-specific contact matrices and user-specified
#' infection parameter values.
#' @inheritParams daedalus
#' @return A list of model parameters suitable for the country or territory
#' specified in `country` and for the infection specified in `epidemic`, with
#' any user-specified over-rides.
#' @keywords internal
make_country_parameters <- function(country, country_params_manual) {
  # NOTE: country name is input checked in top-level fn `daedalus()`
  country_params <- daedalus::country_data[[country]]
  demography <- country_params[["demography"]]
  economic_contacts <- daedalus::economic_contacts

  # NOTE: scaling by leading eigenvalue needed to parameterise transmission
  # using r0 - scaling could be moved to data prep step if data are not to be
  # served to users and are purely internal
  cm <- country_params[["contact_matrix"]]
  # workplace contacts within sectors
  cmw <- economic_contacts[["contacts_workplace"]]

  # NOTE: default assumption for consumer to worker contacts
  # is workplace contacts `cmw` distributed in proportion to demography
  cmcw <- matrix(
    cmw, N_ECON_SECTORS, N_AGE_GROUPS
  ) * demography / sum(demography)

  # NOTE: dummy CM for between-sector contacts; see data-raw/economic_contacts.R
  cm_ww <- economic_contacts[["contacts_between_sectors"]]

  country_params <- list(
    demography = demography,
    contact_matrix = cm,
    contacts_workplace = cmw,
    contacts_consumer_worker = cmcw,
    contacts_between_sectors = cm_ww
  )

  is_empty_list <- checkmate::test_list(country_params_manual, len = 0)
  # check all user inputs
  if (!is_empty_list) {
    allowed_subs <- c(
      "contact_matrix", "contacts_workplace",
      "contacts_consumer_worker", "contacts_between_sectors"
    )
    # check for user values that cannot be used and remove
    is_good_subs <- checkmate::test_subset(
      names(country_params_manual), allowed_subs
    )

    if (!is_good_subs) {
      cli::cli_abort(
        c(
          "`country_params_manual` found the following parameters that are not
          allowed to be passed:
          {.str
            {setdiff(names(country_params_manual), names(country_params))}
          }.",
          i = "`country_params_manual` only allows substituting the following
          contact parameters: {.str {allowed_subs}}"
        ),
        call = parent.frame() # to show parent env rather than helper fn
      )
    }


    if ("contact_matrix" %in% names(country_params_manual)) {
      is_good_cm <- checkmate::test_matrix(
        country_params_manual[["contact_matrix"]],
        mode = "numeric",
        any.missing = FALSE
      )
      if (!is_good_cm) {
        cli::cli_abort(
          "Expected user-provided `contact_matrix` to be a numeric matrix with
          no missing elements."
        )
      }

      is_good_cm_dims <- checkmate::test_matrix(
        country_params_manual[["contact_matrix"]],
        nrows = N_AGE_GROUPS, ncols = N_AGE_GROUPS
      )

      if (!is_good_cm_dims) {
        cli::cli_abort(
          c(
            "Expected user-provided `contact_matrix` to be a square matrix with
            {N_AGE_GROUPS} rows and columns, but it had
            {nrow(country_params_manual[['contact_matrix']])} rows and
            {nrow(country_params_manual[['contact_matrix']])} columns.",
            i = "Number of rows and columns correspond to the number of age
            groups."
          )
        )
      }
    }

    if ("contacts_workplace" %in% names(country_params_manual)) {
      is_good_cw <- checkmate::test_numeric(
        country_params_manual[["contacts_workplace"]],
        len = N_ECON_SECTORS,
        lower = 0.0, finite = TRUE, any.missing = FALSE
      )

      if (!is_good_cw) {
        cli::cli_abort(
          c(
            "Expected user-provided `contacts_workplace` to be a numeric vector
            with {N_ECON_SECTORS} finite elements >= 0.0, but it had
            {length(country_params_manual[['contacts_workplace']])} elements",
            i = "The length of the vector corresponds to the number of economic
            sectors in the model."
          )
        )
      }
    }

    if ("contacts_consumer_worker" %in% names(country_params_manual)) {
      is_good_cmcw <- checkmate::test_matrix(
        country_params_manual[["contacts_consumer_worker"]],
        mode = "numeric",
        any.missing = FALSE
      ) && checkmate::test_numeric(
        country_params_manual[["contacts_consumer_worker"]],
        lower = 0.0,
        finite = TRUE
      )

      is_good_cmcw_dims <- checkmate::test_matrix(
        country_params_manual[["contacts_consumer_worker"]],
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
            {nrow(country_params_manual[['contacts_consumer_worker']])} rows and
            {ncol(country_params_manual[['contacts_consumer_worker']])}
            columns.",
            i = "The number of rows corresponds to the number of economic
            sectors, and the number of colums to the number of demographic
            groups."
          )
        )
      }
    }

    if ("contacts_between_sectors" %in% names(country_params_manual)) {
      is_good_cm_ww <- checkmate::test_matrix(
        country_params_manual[["contacts_between_sectors"]],
        mode = "numeric", any.missing = FALSE
      ) && checkmate::test_numeric(
        country_params_manual[["contacts_between_sectors"]],
        lower = 0.0, finite = TRUE
      )

      is_good_cm_ww_dims <- checkmate::test_matrix(
        country_params_manual[["contacts_between_sectors"]],
        nrows = N_ECON_SECTORS, ncols = N_ECON_SECTORS
      )

      is_zero_diagonal <- all(
        diag(country_params_manual[["contacts_between_sectors"]]) == 0.0
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

      # scale contacts between sectors by the eigenvalue for the NGM
      country_params_manual[["contacts_between_sectors"]] <-
        country_params_manual[["contacts_between_sectors"]] /
          max(Re(
            eigen(country_params_manual[["contacts_between_sectors"]])$values
          ))
    }

    # replace defaults with user values
    country_params[names(country_params_manual)] <- country_params_manual
  }

  # process contact matrices after substituting user-passed values if any
  cm <- country_params[["contact_matrix"]]
  cmw <- country_params[["contacts_workplace"]]
  cmcw <- country_params[["contacts_consumer_worker"]]
  cm_ww <- country_params[["contacts_between_sectors"]]

  cm <- (cm / max(Re(eigen(cm, only.values = TRUE)$values)))
  cm <- cm %*% diag(1 / demography)
  cmw <- cmw / max(Re(eigen(diag(cmw))$values))
  # NOTE: scaling by largest singular value using base::svd, accessed as "d"
  cmcw <- cmcw / max(Re(svd(cmcw)[["d"]]))

  country_params[["contact_matrix"]] <- cm
  country_params[["contacts_workplace"]] <- cmw
  country_params[["contacts_consumer_worker"]] <- cmcw

  # return parameters
  country_params
}

#' @title Make infection parameters list for DAEDALUS
#'
#' @name make_model_parameters
#' @keywords internal
make_infection_parameters <- function(epidemic, infect_params_manual) {
  # NOTE: epidemic is tested in top level function
  # get infection params from `infection_data`
  infection_params <- daedalus::infection_data[[epidemic]]

  is_good_subs <- checkmate::test_subset(
    names(infect_params_manual),
    names(infection_params),
    empty.ok = TRUE
  )

  if (!is_good_subs) {
    cli::cli_abort(
      "`infect_params_manual` found parameters that are not allowed:
      {.str {setdiff(names(infect_params_manual), names(infection_params))}}."
    )
  }

  is_empty_list <- checkmate::test_list(infect_params_manual, len = 0)
  # check all user inputs
  if (!is_empty_list) {
    # NOTE: some infection parameters must be numbers (numeric length 1)
    # while others are numeric vectors of length `N_AGE_GROUPS`
    allowed_numerics_names <- c("eta", "gamma_H", "omega")
    is_each_number <- all(
      vapply(
        infect_params_manual[!names(infect_params_manual) %in%
          allowed_numerics_names],
        checkmate::test_number,
        logical(1L),
        # NOTE: rate parameter limits allowed may need to be tweaked
        lower = 0.0,
        finite = TRUE
      )
    )

    is_numeric_good <- all(
      vapply(
        infect_params_manual[allowed_numerics_names], checkmate::test_numeric,
        logical(1L),
        len = N_AGE_GROUPS, lower = 0.0,
        finite = TRUE, null.ok = TRUE
      )
    )

    if (!is_each_number) {
      cli::cli_abort(
        c(
          "Expected the following parameters passed in `infect_params_manual`
          to be a single positive and finite number:
          {.str {setdiff(
            names(infect_params_manual), allowed_numerics_names
          )}}",
          i = "Only {.str {allowed_numerics_names}} may be numeric vectors.
          See the Help page for {.help daedalus::daedalus} for parameters that
          can be numerics."
        )
      )
    }

    if (!is_numeric_good) {
      cli::cli_abort(
        c(
          "Expected the following parameters passed in `infect_params_manual`
          to be numeric vectors of length {N_AGE_GROUPS} with positive and
          finite values:
          {.str
            {intersect(names(infect_params_manual), allowed_numerics_names)}
          }",
          i = "See the Help page for {.help daedalus::daedalus} for parameters
          that can be numerics."
        )
      )
    }
  }

  infection_params[names(infect_params_manual)] <- infect_params_manual

  infection_params
}
