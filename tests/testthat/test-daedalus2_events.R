test_that("daedalus2: root-finding events launch at each appropriate root", {
  # check that NPI triggers at response time when response time is low
  response_time <- 22.0
  output <- daedalus(
    "THA",
    "sars_cov_1",
    response_strategy = "elimination",
    response_time = response_time,
    time_end = 600
  )

  expect_identical(
    output$response_data$closure_info$closure_time_start,
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
    output$response_data$closure_info$closure_time_start,
    response_time
  )
})

test_that("daedalus2: setting response duration works", {
  response_time <- 10
  response_duration <- 30
  output <- daedalus(
    "GBR",
    "sars_cov_1",
    "elimination",
    response_time = response_time,
    response_duration = response_duration
  )

  expect_identical(
    output$response_data$closure_info$closure_time_end,
    response_time + response_duration
  )

  # check that response duration of 0 is the same as no response
  # must set hospital capacity to prevent event based triggering
  cx <- daedalus_country("GBR")
  cx$hospital_capacity <- 1e9
  response_time <- 10
  response_duration <- 0
  output_alt <- daedalus(
    cx,
    "sars_cov_1",
    "elimination",
    response_time = response_time,
    response_duration = response_duration
  )
  output <- daedalus(
    cx,
    "sars_cov_1"
  )

  expect_identical(
    get_epidemic_summary(output_alt, "infections"),
    get_epidemic_summary(output, "infections")
  )
})

# NOTE: this test will/should be reinstated when daedalus() replaces daedalus()
skip("Full event data is no longer returned for compliance with output class")
test_that("Vaccination events launch as expected", {
  # expect vaccination is launched if chosen and does not end
  vax_time <- 33
  v <- daedalus_vaccination("high", start_time = vax_time)
  output <- daedalus(
    x,
    "sars_cov_1",
    vaccine_investment = v,
    time_end = 100
  )

  checkmate::expect_subset(
    "vaccination_time_on",
    output$event_data$name
  )
  expect_identical(
    output$event_data[output$event_data$name == "vaccination_time_on", "time"],
    vax_time
  )

  expect_false(
    checkmate::test_subset(
      "vaccination_time_off",
      output$event_data$name
    )
  )
})
