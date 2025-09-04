# Tests for the <daedalus_npi> class

test_that("class <daedalus_npi>: basic expectations", {
  expect_no_condition(
    daedalus_npi("elimination", "GBR", "sars_cov_1")
  )

  expect_no_condition(
    lapply(
      daedalus.data::closure_strategy_names,
      daedalus_npi,
      country = "GBR",
      infection = "sars_cov_1"
    )
  )

  cty <- "GBR"
  infection <- "influenza_1918"
  npi <- daedalus_npi("school_closures", cty, infection)
  expect_no_condition(
    daedalus(cty, infection, npi)
  )

  expect_snapshot(npi)

  expect_no_condition(
    get_data(npi, "openness")
  )

  npi <- daedalus_npi(
    NA,
    cty,
    infection,
    rep(0.5, 45)
  )
  expect_no_condition(
    daedalus(cty, infection, npi)
  )
})

test_that("class <daedalus_npi>: class validation", {
  country <- "GBR"
  infection <- "sars_cov_1"
  x <- daedalus_npi("elimination", country, infection)
  class(x) <- "dummy_class"

  expect_error(
    validate_daedalus_npi(x),
    "(daedalus\\_vaccination)*(check class assignment)"
  )

  x <- daedalus_npi("elimination", country, infection)
  x$parameters$dummy_param <- NA_character_
  expect_error(
    validate_daedalus_npi(x),
    "(is class)*(daedalus\\_vaccination)*(does not have the correct attributes)"
  )
})

test_that("class <daedalus_npi>: sequential time-limited NPIs", {
  cty <- "GBR"
  infection <- "influenza_1918"
  npi <- daedalus_npi(
    "school_closures",
    cty,
    infection,
    start_time = c(30, 90),
    end_time = c(50, 50)
  )

  expect_snapshot(npi)

  expect_no_condition(
    daedalus(cty, infection, npi)
  )

  npi <- daedalus_npi(
    NA,
    cty,
    infection,
    rep(0.5, 45),
    start_time = c(30, 90),
    end_time = c(50, 120)
  )
  cty <- daedalus_country("GBR")

  expect_no_condition(
    daedalus(cty, infection, npi)
  )
})

test_that("daedalus: time-launched response duration is correct", {
  # duration is based on user-specified start time
  # prevent state-launched NPIs
  response_time <- 10
  end_time <- 40
  cty <- daedalus_country("GBR")
  cty$hospital_capacity <- 1e9
  npi <- daedalus_npi(
    "elimination",
    cty,
    "sars_cov_1",
    start_time = response_time,
    end_time = end_time
  )
  output <- daedalus(
    cty,
    "sars_cov_1",
    npi
  )

  expect_identical(
    output$response_data$closure_info$closure_times_end,
    end_time
  )
  expect_identical(
    output$response_data$closure_info$closure_durations,
    end_time - response_time
  )
})

test_that("class <daedalus_npi>: state-dependent event launch correctly", {
  # check that NPI triggers at response time when response time is low
  response_time <- 22.0
  x <- daedalus_country("THA")
  x$hospital_capacity <- 1e8 # artificially high

  output <- daedalus(
    x,
    "sars_cov_1",
    response_strategy = "elimination",
    response_time = response_time
  )

  expect_identical(
    output$response_data$closure_info$closure_times_start,
    response_time
  )

  # check that NPI triggers when hospital capacity is crossed when
  # response time is high
  response_time <- 50
  x <- daedalus_country("THA")
  x$hospital_capacity <- 1e2 # artificially low; resp launches around t = 40
  output <- daedalus(
    x,
    "sars_cov_1",
    response_strategy = "elimination",
    response_time = response_time,
    time_end = 600
  )
  expect_lt(
    output$response_data$closure_info$closure_times_start,
    response_time
  )
})

# NOTE: this test only passes under a specific set of conditions
# because state-launched NPIs can be triggered multiple times
test_that("class <daedalus_npi>: state-launched response duration is correct", {
  # duration is based on state-driven start time
  # NOTE: model only 100 days to avoid secondary peaks
  start_time <- 100
  max_response_duration <- 30
  npi <- daedalus_npi(
    "elimination",
    "GBR",
    "sars_cov_2_delta",
    start_time = start_time, # prevent time-based trigger
    max_duration = max_response_duration
  )
  output <- daedalus(
    "GBR",
    "sars_cov_2_delta",
    npi,
    time_end = 100
  )

  expect_identical(
    sum(output$response_data$closure_info$closure_durations),
    max_response_duration,
    tolerance = 1e-6
  )
})

test_that("class <daedalus_npi>: throws expected errors", {
  expect_error(
    daedalus_npi(
      "elimination",
      "THA",
      "sars_cov_1",
      rep(1, 45)
    ),
    "pre-defined NPI strategy passed to `name` and custom `openness` values"
  )

  expect_error(
    daedalus_npi(
      NA_character_,
      "THA",
      "sars_cov_1",
      rep(2, 45)
    ),
    "values between 0.0 and 1.0"
  )

  expect_error(
    get_data(
      daedalus_npi("elimination", "THA", "sars_cov_1"),
      "dummy_param"
    ),
    "`to_get` must be a string available"
  )

  expect_error(
    validate_daedalus_npi(
      daedalus_country("GBR")
    ),
    "check class assignment"
  )

  x <- daedalus_npi(NA, "THA", "sars_cov_1", rep(0.1, 45))
  x$parameters$openness <- rep(2, 34)
  expect_error(
    validate_daedalus_npi(x),
    "must be a numeric of length 45, with values between 0.0 and 1.0"
  )

  x <- list(parameters = list(openness = rep(1, 45), dummy = NA))
  class(x) <- "daedalus_npi"
  expect_error(
    validate_daedalus_npi(x),
    "does not have the correct attributes"
  )

  expect_error(
    validate_npi_input(
      list(dummy = "dummy"),
      "THA",
      "sars_cov_1",
      30,
      60
    ),
    "Got an unexpected value of class"
  )
})
