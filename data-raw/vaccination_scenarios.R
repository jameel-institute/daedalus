## code to prepare `vaccination_scenarios` dataset goes here
# NOTE: advance vaccination investment scenarios are adapted from scenarios
# developed for a report for CEPI.

# vaccination rollout start time
start_times <- list(none = 365, low = 300, medium = 200, high = 100)

# rates are represented in percentage population per week
vaccination_rate_time_unit <- 7
vaccination_rate <- list(none = 1, low = 2, medium = 3, high = 3.5)

# vaccination uptake limit
uptake_limit <- list(none = 40, low = 50, medium = 60, high = 80)

# save names and combine data
vaccination_scenario_names <- c("none", "low", "medium", "high")

# NOTE: efficacy and waning rate are not scenario specific
vaccination_parameter_names <- c(
  "start_time",
  "rate",
  "uptake_limit",
  "efficacy",
  "waning_period"
)

# default efficacy and waning period
efficacy <- 50 # 50% efficacy
waning_period <- 270 # waning period in days

vaccination_scenario_data <- Map(
  start_times,
  vaccination_rate,
  uptake_limit,
  f = function(t, nu, limit) {
    x <- list(
      t,
      nu / vaccination_rate_time_unit,
      limit,
      efficacy,
      waning_period
    )
    names(x) <- vaccination_parameter_names

    x
  }
)
names(vaccination_scenario_data) <- vaccination_scenario_names

usethis::use_data(vaccination_scenario_data, overwrite = TRUE)
usethis::use_data(vaccination_scenario_names, overwrite = TRUE)
usethis::use_data(vaccination_parameter_names, overwrite = TRUE)
