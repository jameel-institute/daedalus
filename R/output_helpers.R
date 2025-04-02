#' Calculate daily incidences and summarise epidemic measures
#'
#' @name epi_output_helpers
#' @rdname epi_output_helpers
#'
#' @description Functions to quickly summarise timeseries data from `daedalus()`
#' to provide daily values for infections, hospitalisations, deaths, and
#' vaccinations, while allowing grouping by different strata.
#'
#' @param data Either a `<data.frame>` from a call to `get_data()` on a
#' `<daedalus_output>` object, or such an object directly.
#'
#' @param measures A character vector of one or more of the following, passed to
#' `get_incidence()` and `get_epidemic_summary()`:
#' `"infections"`, `"hospitalisations"` or `"deaths"` for the
#' measure to return. Defaults to returning all three in long format.
#'
#' `get_daily_vaccinations()` does not accept a `measures` argument and only
#' provides the number of daily vaccinations.
#'
#' @param groups An optional character vector of grouping variables that
#' correspond to model strata. Defaults to `NULL` which gives incidence across
#' the whole population. Allowed groups correspond to modelled strata:
#' `"age_group"`, `"vaccine_group"`, and `"econ_sector"`.
#'
#' `get_daily_vaccinations()` only accepts "`age_group`" and `"econ_sector"`.
#'
#' @return A `<data.frame>` in long format, with one entry per
#' model timestep, measure, and group chosen.
#'
#' - `get_incidence()` returns a data frame with the number of daily new
#' infections, new hospitalisations, and/or new deaths in each of the groups
#' specified by `groups`.
#'
#' - `get_epidemic_summary()` returns a data frame with the total number of the
#' value specified in `measure` for each of the groups specified by `groups`.
#'
#' - `get_daily_vaccinations()` returns a data frame with columns for the
#' number of new daily vaccination in each combination of `groups` if provided.
#' Columns for the `groups` are added when `groups` are specified.
#'
#' @examples
#' data <- daedalus("Canada", "sars_cov_1")
#'
#' # new infections
#' new_infections <- get_incidence(data, "infections")
#'
#' # epidemic summary
#' get_epidemic_summary(
#'   data,
#'   groups = "age_group"
#' )
#'
#' # get daily vaccinations
#' daily_vaccinations <- get_new_vaccinations(data)
#'
#' @export
get_incidence <- function(
  data,
  measures = c("infections", "hospitalisations", "deaths"),
  groups = NULL
) {
  # set global variables to NULL
  value <- NULL
  compartment <- NULL
  measure <- NULL

  # check data
  is_good_data <- checkmate::test_data_frame(data, any.missing = FALSE) ||
    checkmate::test_class(data, "daedalus_output")

  if (!is_good_data) {
    cli::cli_abort(
      "Expected `data` to be either a `data.frame` or a
        {.cls daedalus_output} object."
    )
  } else if (is_daedalus_output(data)) {
    data <- get_data(data)
  }

  # check measures and groups
  measures <- rlang::arg_match(measures, SUMMARY_MEASURES, multiple = TRUE)

  is_good_groups <- checkmate::test_subset(groups, SUMMARY_GROUPS)
  if (!is_good_groups) {
    cli::cli_abort(
      "Expected `groups` to be either `NULL` or one or more of
        {.str {SUMMARY_GROUPS}}."
    )
  }

  # subset compartments needed
  compartments_needed <- c("new_infections", "new_hosp", "dead")
  df_measures <- data.frame(
    compartment = compartments_needed,
    measure = SUMMARY_MEASURES
  )
  df_measures <- df_measures[df_measures$measure %in% measures, ]

  # subset new infections compartment
  dt_new <- data[data$compartment %in% df_measures$compartment, ]
  data.table::setDT(dt_new)

  # aggregate over grouping variables and run diff()
  dt_new <- dt_new[,
    list(value = sum(value)),
    by = c("time", "compartment", groups)
  ]
  dt_new[, value := c(0, diff(value)), by = c(groups, "compartment")]

  # rename compartments
  dt_new[,
    measure := data.table::fcase(
      compartment == "dead",
      "daily_deaths",
      compartment == "new_infections",
      "daily_infections",
      compartment == "new_hosp",
      "daily_hospitalisations"
    )
  ]

  data.table::setDF(dt_new)
  dt_new[, setdiff(colnames(dt_new), "compartment")]
}

#' @name epi_output_helpers
#' @export
get_epidemic_summary <- function(
  data,
  measures = c("infections", "hospitalisations", "deaths"),
  groups = NULL
) {
  # set global variables to NULL
  value <- NULL
  compartment <- NULL
  measure <- NULL

  is_good_data <- checkmate::test_data_frame(data, any.missing = FALSE) ||
    checkmate::test_class(data, "daedalus_output")

  if (!is_good_data) {
    cli::cli_abort(
      "Expected `data` to be either a `data.frame` or a
        {.cls daedalus_output} object."
    )
  } else if (is_daedalus_output(data)) {
    data <- get_data(data)
  }

  # check measures and groups
  measures <- rlang::arg_match(measures, SUMMARY_MEASURES, multiple = TRUE)

  is_good_groups <- checkmate::test_subset(groups, SUMMARY_GROUPS)
  if (!is_good_groups) {
    cli::cli_abort(c(
      "Expected `groups` to be either `NULL` or a character vector of
        model groups.",
      i = "Allowed groups are {.str {SUMMARY_GROUPS}}."
    ))
  }

  # subset compartments needed
  compartments_needed <- c("new_infections", "new_hosp", "dead")
  df_measures <- data.frame(
    compartment = compartments_needed,
    measure = SUMMARY_MEASURES
  )
  df_measures <- df_measures[df_measures$measure %in% measures, ]

  # subset new infections compartment
  dt_summary <- data[data$compartment %in% df_measures$compartment, ]
  data.table::setDT(dt_summary)

  # aggregate over grouping variables and get the final value
  dt_summary <- dt_summary[,
    list(value = sum(value)),
    by = c("time", "compartment", groups)
  ]

  dt_summary <- dt_summary[,
    list(value = data.table::last(value)),
    by = c("compartment", groups)
  ]

  dt_summary[,
    measure := data.table::fcase(
      compartment == "dead",
      "total_deaths",
      compartment == "new_infections",
      "epidemic_size",
      compartment == "new_hosp",
      "total_hospitalisations"
    )
  ]

  data.table::setDF(dt_summary)
  dt_summary[, setdiff(colnames(dt_summary), "compartment")]
}

#' @name epi_output_helpers
#' @export
get_new_vaccinations <- function(data, groups = NULL) {
  # set global variables to NULL
  value <- NULL

  # check data
  is_good_data <- checkmate::test_data_frame(data, any.missing = FALSE) ||
    checkmate::test_class(data, "daedalus_output")

  if (!is_good_data) {
    cli::cli_abort(
      "Expected `data` to be either a `data.frame` or a
        {.cls daedalus_output} object."
    )
  } else if (is_daedalus_output(data)) {
    data <- get_data(data)
  }

  # NOTE: allowed groups are different from SUMMARY_GROUPS as `vaccine_group`
  # is not allowed
  allowed_groups <- setdiff(SUMMARY_GROUPS, "vaccine_group")

  is_good_groups <- checkmate::test_subset(groups, allowed_groups)
  if (!is_good_groups) {
    cli::cli_abort(c(
      "Expected `groups` to be either `NULL` or a character vector of
        model groups.",
      i = "Allowed groups are {.str {allowed_groups}}."
    ))
  }

  dt_new <- data[data$vaccine_group == "new_vaccinations", ]
  data.table::setDT(dt_new)

  dt_new <- dt_new[, list(value = sum(value)), by = c("time", groups)]
  dt_new[, "new_vaccinations" := c(0, diff(value)), by = groups]

  data.table::setDF(dt_new)
  dt_new[, setdiff(colnames(dt_new), "value")]
}

#' Get life-years lost by demographic group.
#'
#' @param output A `<daedalus_output>` object.
#' @param groups Whether to get the life-years lost by age group. Selecting
#' `"none"` gives the total life-years lost.
#'
#' @return A `<data.frame>` with the number of life-years lost, optionally per
#' age-group.
#'
#' @export
get_life_years_lost <- function(output, groups = c("none", "age_group")) {
  checkmate::assert_class(output, "daedalus_output")
  groups <- rlang::arg_match(groups)
  df <- get_epidemic_summary(output, "deaths", "age_group")

  life_expectancy <- get_data(output, "country_parameters")[["life_expectancy"]]

  df$value <- df$value * life_expectancy
  df$measure <- "life_years_lost"

  switch(
    groups,
    none = {
      data.frame(value = sum(df$value), measure = factor("life_years_lost"))
    },
    age_group = df
  )
}
