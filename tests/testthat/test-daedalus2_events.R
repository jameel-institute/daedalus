# check for vaccination mechanism
test_that("daedalus2: events do not start at model start time", {
  # test on events starting at model start time
  vax <- daedalus_vaccination("none", 0, 0.5, 100)
  output <- daedalus2("THA", "sars_cov_1", vaccine_investment = vax)

  # expect vaccination group is zero
  expect_identical(max(output$S_vax), 0.0)
})

test_that("daedalus2: root-finding events launch at each appropriate root", {
  # check that NPI triggers at both response time and when hosp capacity is
  # crossed
  response_time <- 22.0
  output <- daedalus2(
    "THA",
    "sars_cov_1",
    response_strategy = "elimination",
    response_time = response_time,
    time_end = 600
  )

  expect_identical(
    output$event_data[output$event_data$name == "npi_time_on", "time"],
    response_time
  )

  checkmate::expect_subset(
    "npi_state_on",
    output$event_data$name,
  )

  # expect no vaccination is launched
  expect_false(
    checkmate::test_subset(
      "vax_time_on",
      output$event_data$name
    )
  )

  # expect response is launched only at a specific time when hosp capacity
  # is high
  x <- daedalus_country("THA")
  x$hospital_capacity <- 1e9 # artificially high
  output <- daedalus2(
    x,
    "sars_cov_1",
    response_strategy = "elimination",
    response_time = response_time,
    time_end = 600
  )

  expect_false(
    checkmate::test_subset(
      "npi_state_on",
      output$event_data$name
    )
  )

  # expect event ends at defined time
  default_duration <- 60.0
  expect_identical(
    diff(output$event_data$time),
    default_duration
  )

  # expect vaccination is launched if chosen and does not end
  vax_time <- 33
  v <- daedalus_vaccination("high", start_time = vax_time)
  output <- daedalus2(
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
