# Tests for the hospital capacity mechanism
# check that models run when hospital capacity is specified
test_that("Hospital capacity: basic expectations", {
  # NOTE: Not testing every country and infection
  response_strategy <- c("elimination", "school_closures", "economic_closures")
  cx <- daedalus_country("China")
  cx$hospital_capacity <- 100
  invisible(lapply(response_strategy, function(x) {
    expect_no_condition({
      daedalus(
        country = cx,
        infection = "influenza_1918",
        response_strategy = x
      )
    })
  }))
})

# check that increasing hospital capacity leads to later closure
test_that("Closures: hospital capacity and closure time", {
  # hospital capacity saved in country class
  cty_x <- daedalus_country("Canada")
  cty_y <- daedalus_country("Canada")
  cty_y$hospital_capacity <- round(cty_y$hospital_capacity * 2)

  x <- daedalus(
    cty_x,
    "sars_cov_1",
    response_strategy = "elimination",
    response_time = 101, # prevent auto-response
    time_end = 100
  )
  y <- daedalus(
    cty_y,
    "sars_cov_1",
    response_strategy = "elimination",
    response_time = 101, # prevent auto-response
    time_end = 100
  )

  expect_lt(
    x$response_data$npi_info$npi_times_start,
    y$response_data$npi_info$npi_times_start
  )

  # hospital capacity override from `daedalus()`
  cty_x$hospital_capacity <- cty_y$hospital_capacity * 2 # 4x higher
  x <- daedalus(
    cty_x,
    "sars_cov_1",
    response_strategy = "elimination",
    response_time = 101, # no auto response
    time_end = 100
  )

  expect_gt(
    x$response_data$npi_info$npi_times_start,
    y$response_data$npi_info$npi_times_start
  )
})

test_that("Deaths increase when hospital capacity is exceeded", {
  cty <- daedalus_country("GBR")
  cty$hospital_capacity <- 1e2
  output <- daedalus(cty, "sars_cov_1", time_end = 300)

  cty$hospital_capacity <- 1e5 # max possible
  output2 <- daedalus(cty, "sars_cov_1", time_end = 300)

  deaths <- get_epidemic_summary(output, "deaths")
  deaths2 <- get_epidemic_summary(output2, "deaths")

  expect_lt(
    deaths2$value,
    deaths$value
  )
})
