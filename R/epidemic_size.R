#' Get derived epidemic data
#'
#' @name epidemic_summary
#' @rdname epidemic_summary
#'
#' @description Functions to quickly summarise timeseries data from `daedalus()`
#' while allowing grouping by different strata.
#'
#' @param data Either a `<data.frame>` or a `<daedalus_output>` object.
#' @param grouping_vars An optional character vector of grouping variables that
#' correspond to model strata. Defaults to `NULL`, with allowed values of
#' `age_group`, `vaccine_group`, and `econ_sector`.
#' @param measure A character vector of one or more of the following:
#' `"epidemic_size"`, `"total_hospitalisations"` or `"total_deaths"` for the
#' measure to return.
#'
#' @return A `<data.frame>`.
#'
#' - `get_new_infections()` returns a data frame with the number of daily new
#' infections in each of the groups specified by `grouping_vars`.
#'
#' - `get_epidemic_summary()` returns a data frame with the measure specified
#' in `measure` for each of the groups specified by `grouping_vars`.
#'
#' @examples
#' data <- daedalus("Canada", "sars_cov_1")
#'
#' # new infections
#' new_infections <- get_new_infections(data)
#'
#' # epidemic summary
#' get_epidemic_summary(
#'   data,
#'   measure = c("epidemic_size", "total_deaths", "total_hospitalisations"),
#'   grouping_vars = "age_group"
#' )
#'
#' @export
get_new_infections <- function(data, grouping_vars = NULL) {
  # set global variables to NULL
  value <- NULL
  new_infections <- NULL

  is_good_data <- checkmate::test_data_frame(
    data,
    any.missing = FALSE
  ) || checkmate::test_class(data, "daedalus_output")

  if (!is_good_data) {
    cli::cli_abort(
      "Expected `data` to be either a `data.frame` or a
        {.cls daedalus_output} object."
    )
  } else if (is_daedalus_output(data)) {
    data <- get_data(data)
  }

  allowed_groups <- c("age_group", "vaccine_group", "econ_sector")
  checkmate::assert_subset(
    grouping_vars,
    allowed_groups,
    empty = TRUE
  )

  # subset new infections compartment
  dt_new_inf <- data[data$compartment == "new_infections", ]
  data.table::setDT(dt_new_inf)

  # aggregate over grouping variables and run diff()
  dt_new_inf <- dt_new_inf[,
    list(value = sum(value)),
    by = c("time", grouping_vars)
  ]
  dt_new_inf[, new_infections := c(0, diff(value)),
    by = grouping_vars
  ]

  data.table::setDF(dt_new_inf)
  dt_new_inf[, setdiff(colnames(dt_new_inf), "time")]
}

#' @name epidemic_summary
#' @export
get_epidemic_summary <- function(
    data,
    measure = c(
      "epidemic_size", "total_hospitalisations",
      "total_deaths"
    ),
    grouping_vars = NULL) {
  # set global variables to NULL
  value <- NULL
  is_good_data <- checkmate::test_data_frame(
    data,
    any.missing = FALSE
  ) || checkmate::test_class(data, "daedalus_output")

  if (!is_good_data) {
    cli::cli_abort(
      "Expected `data` to be either a `data.frame` or a
        {.cls daedalus_output} object."
    )
  } else if (is_daedalus_output(data)) {
    data <- get_data(data)
  }

  allowed_measures <- c(
    "epidemic_size", "total_hospitalisations", "total_deaths"
  )
  is_good_measure <- checkmate::test_subset(
    measure, allowed_measures,
    empty.ok = FALSE
  )
  if (!is_good_measure) {
    cli::cli_abort(
      c(
        "Found unexpected value for `measure`.",
        i = "Allowed `measure` vector must be one or more of
          {.str {allowed_measures}}"
      )
    )
  }

  allowed_groups <- c("age_group", "vaccine_group", "econ_sector")
  checkmate::assert_subset(
    grouping_vars,
    allowed_groups,
    empty = TRUE
  )

  # get compartments to summarise
  compartments_needed <- c("new_infections", "new_hosp", "dead")
  measures <- c(
    "epidemic_size", "total_hospitalisations", "total_deaths"
  )
  df_measures <- data.frame(
    compartment = compartments_needed,
    measure = measures
  )
  df_measures <- df_measures[df_measures$measure %in% measure, ]

  # subset new infections compartment
  dt_summary <- data[data$compartment %in% compartments_needed, ]
  data.table::setDT(dt_summary)

  # aggregate over grouping variables and run diff()
  dt_summary <- dt_summary[, list(value = sum(value)),
    by = c("time", "compartment", grouping_vars)
  ]

  dt_summary <- dt_summary[, list(value = data.table::last(value)),
    by = c("compartment", grouping_vars)
  ]

  dt_summary <- merge(dt_summary, df_measures)

  data.table::setDF(dt_summary)
  dt_summary[, c("measure", "value")]
}
