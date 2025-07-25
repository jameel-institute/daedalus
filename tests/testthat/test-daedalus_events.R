test_that("daedalus: root-finding events launch at the appropriate root", {
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

test_that("daedalus: time-launched response duration is correct", {
  # duration is based on user-specified start time
  # prevent state-launched NPIs
  response_time <- 10
  response_duration <- 30
  cty <- daedalus_country("GBR")
  cty$hospital_capacity <- 1e9
  output <- daedalus(
    cty,
    "sars_cov_1",
    "elimination",
    response_time = response_time,
    response_duration = response_duration
  )

  expect_identical(
    output$response_data$closure_info$closure_times_end,
    response_time + response_duration
  )
  expect_identical(
    output$response_data$closure_info$closure_durations,
    response_duration
  )
})

# NOTE: this test only passes under a specific set of conditions
# because state-launched NPIs can be triggered multiple times
test_that("daedalus: state-launched response duration is correct", {
  # duration is based on state-driven start time
  # NOTE: model only 100 days to avoid secondary peaks
  response_duration <- 30
  output <- daedalus(
    "GBR",
    "sars_cov_2_delta",
    "elimination",
    response_time = 100, # artificially high
    response_duration = response_duration,
    time_end = 100
  )

  expect_identical(
    sum(output$response_data$closure_info$closure_durations),
    response_duration,
    tolerance = 1e-6
  )
})

test_that("Vaccination events launch and end as expected", {
  # expect vaccination is launched if chosen and does not end
  cty <- "THA"
  vax_time <- 33
  v <- daedalus_vaccination("low", cty, start_time = vax_time)
  expected_vaccinations <- v$value_state_off

  output <- daedalus(
    cty,
    "sars_cov_1",
    vaccine_investment = v
  )

  checkmate::expect_subset(
    "vaccination_time_on",
    output$event_data$name
  )
  expect_identical(
    output$event_data[output$event_data$name == "vaccination_time_on", "time"],
    vax_time
  )

  total_vaccinations <- sum(get_new_vaccinations(output)$new_vaccinations)
  expect_identical(
    total_vaccinations,
    expected_vaccinations,
    tolerance = 1e-6
  )
})
