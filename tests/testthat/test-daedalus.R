# Basic tests for DAEDALUS
# select a test country
country_canada <- "Canada"

test_that("daedalus: basic expectations", {
  time_end <- 100L
  # expect no conditions
  expect_no_condition({
    output <- daedalus(country_canada, "influenza_1918", time_end = time_end)
  })

  # expect type double and non-negative
  expect_s3_class(output, "daedalus_output")
  data <- get_data(output)
  expect_length(data, N_OUTPUT_COLS)

  # as non-working groups do not have data per sector
  expected_rows <- time_end * N_EPI_COMPARTMENTS *
    (N_AGE_GROUPS + N_ECON_SECTORS)

  expect_identical(
    nrow(data),
    expected_rows
  )
  expect_named(
    data,
    c("time", "age_group", "compartment", "econ_sector", "value")
  )
  expect_type(
    data[["time"]], "double"
  )
  expect_type(
    data[["age_group"]], "character"
  )
  expect_type(
    data[["compartment"]], "character"
  )
  expect_type(
    data[["value"]], "double"
  )

  # expect closed population with no change in total size
  # NOTE: disregard first column holding time
  # NOTE: set tolerance to a reasonable value
  expect_identical(
    sum(data[data$time == max(data$time), ]$value),
    sum(data[data$time == min(data$time), ]$value),
    tolerance = 1e-12
  )
})

# test that daedalus runs for all epidemic infection parameter sets
test_that("daedalus: Runs for all country x infection x response", {
  country_infection_combos <- data.table::CJ(
    country = daedalus::country_names,
    infection = daedalus::epidemic_names
  )
  time_end <- 50

  # expect no conditions
  expect_no_condition({
    Map(
      country_infection_combos$country,
      country_infection_combos$infection,
      f = function(x, y) {
        daedalus(x, y)
      }
    )
  })

  # expect classed output, type double, and non-negative
  checkmate::expect_list(output_list, "daedalus_output")
  expect_true(
    all(vapply(output_list, function(x) {
      x <- get_data(x)
      all(x$value >= 0.0)
    }, FUN.VALUE = logical(1)))
  )
})

# test that passing model parameters works
test_that("daedalus: Passing model parameters", {
  expect_no_condition(
    daedalus(
      country_canada,
      daedalus_infection(
        "influenza_1918",
        r0 = 1.3, eta = c(0.1, 0.2, 0.3, 0.4)
      )
    )
  )
})

# test statistical correctness for only the covid wildtype infection param set
test_that("daedalus: statistical correctness", {
  output <- daedalus(country_canada, "influenza_1918")
  data <- get_data(output)
  # tests on single compartment without workers
  data <- data[data$age_group == "65+", ]

  # expectations when immunity wanes allowing R -> S
  # no elegant way of programmatically accessing idx
  deaths <- data[data$compartment == "dead", ]$value
  expect_true(
    all(diff(deaths) >= 0.0)
  )
  susceptibles <- data[data$compartment == "susceptible", ]$value
  expect_false(
    all(diff(susceptibles) <= 0.0)
  )

  recovered <- data[data$compartment == "recovered", ]$value
  expect_false(
    all(diff(recovered) >= 0.0)
  )

  # expectations when immunity does not wane
  # - monotonically decreasing susceptibles
  # - monotonically increasing recovered and deaths
  output <- daedalus(
    country_canada, daedalus_infection("influenza_1918", rho = 0.0)
  )
  data <- get_data(output)
  data <- data[data$age_group == "65+", ]

  susceptibles <- data[data$compartment == "susceptible", ]$value
  expect_true(
    all(diff(susceptibles) <= 0.0)
  )

  recovered <- data[data$compartment == "recovered", ]$value
  expect_true(
    all(diff(recovered) >= 0.0)
  )

  deaths <- data[data$compartment == "dead", ]$value
  expect_true(
    all(diff(deaths) >= 0.0)
  )
})

test_that("daedalus: errors and warnings", {
  # expect errors on country
  expect_error(
    daedalus("U.K.", "influenza_1918"),
    regexp = "`country` must be one of"
  )

  # expect errors on poorly specified time_end
  expect_error(
    daedalus(country_canada, "influenza_1918", time_end = -1),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )
  expect_error(
    daedalus(country_canada, "influenza_1918", time_end = 100.5),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )
  expect_error(
    daedalus(country_canada, "influenza_1918", time_end = Inf),
    regexp = "Expected `time_end` to be a single positive integer-like number."
  )

  expect_error(
    daedalus(country_canada, daedalus_infection("unfluenza_1920"))
  )

  # expect errors on bad response thresholds
  expect_error(
    daedalus(
      country_canada, daedalus_infection("sars_cov_1"),
      response_threshold = 0
    ),
    regexp = "Expected `response_threshold` to be a positive finite integer."
  )
  expect_error(
    daedalus(
      country_canada, daedalus_infection("sars_cov_1"),
      response_threshold = -1
    ),
    regexp = "Expected `response_threshold` to be a positive finite integer."
  )
  expect_error(
    daedalus(
      country_canada, daedalus_infection("sars_cov_1"),
      response_threshold = Inf
    ),
    regexp = "Expected `response_threshold` to be a positive finite integer."
  )
  expect_error(
    daedalus(
      country_canada, daedalus_infection("sars_cov_1"),
      response_threshold = NA_real_
    ),
    regexp = "Expected `response_threshold` to be a positive finite integer."
  )
  expect_error(
    daedalus(
      country_canada, daedalus_infection("sars_cov_1"),
      response_threshold = 10.5
    ),
    regexp = "Expected `response_threshold` to be a positive finite integer."
  )

  # expect errors on bad response time
  expect_error(
    daedalus(
      country_canada, daedalus_infection("sars_cov_1"),
      response_time = -1
    ),
    regexp = "Expected `response_time` to be between 2"
  )
  expect_error(
    daedalus(
      country_canada, daedalus_infection("sars_cov_1"),
      response_time = 30, time_end = 30
    ),
    regexp = "Expected `response_time` to be between 2"
  )
  expect_error(
    daedalus(
      country_canada, daedalus_infection("sars_cov_1"),
      response_time = 30, time_end = 10
    ),
    regexp = "Expected `response_time` to be between 2"
  )
  expect_error(
    daedalus(
      country_canada, daedalus_infection("sars_cov_1"),
      response_time = Inf
    ),
    regexp = "Expected `response_time` to be between 2"
  )
  expect_error(
    daedalus(
      country_canada, daedalus_infection("sars_cov_1"),
      response_time = NA_real_
    ),
    regexp = "Expected `response_time` to be between 2"
  )
  expect_error(
    daedalus(
      country_canada, daedalus_infection("sars_cov_1"),
      response_time = 10.5
    ),
    regexp = "Expected `response_time` to be between 2"
  )
})
