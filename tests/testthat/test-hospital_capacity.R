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

test_that(("Hospital capacity: flag and trigger mechanism"), {
  time_end <- 300
  cty <- daedalus_country("GB")
  out <- daedalus(cty, "sars_cov_2_pre_alpha", time_end = 300)
  hosp_flag_auto <- as.logical(out$ode_soln$hosp_overflow_flag)

  hosp_flag_manual <- colSums(
    out$ode_soln$hospitalised_recov +
      out$ode_soln$hospitalised_death
  ) >
    cty$hospital_capacity

  expect_identical(
    hosp_flag_auto,
    hosp_flag_manual
  )

  # check event is registered
  checkmate::expect_names(
    out$ode_events[[1]]$name,
    must.include = sprintf("hosp_cap_exceeded_state_%s", c("on", "off"))
  )
})

# check that increasing hospital capacity leads to later closure
test_that("Closures: hospital capacity and closure time", {
  # hospital capacity saved in country class
  cty_x <- daedalus_country("Canada")
  cty_y <- daedalus_country("Canada")
  cty_y$hospital_capacity <- round(cty_y$hospital_capacity * 2)

  x <- daedalus(
    cty_x,
    "sars_cov_2_omicron",
    response_strategy = "elimination",
    response_time = 101, # prevent auto-response
    time_end = 100
  )
  y <- daedalus(
    cty_y,
    "sars_cov_2_omicron",
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
    "sars_cov_2_omicron",
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
  output <- daedalus(cty, "sars_cov_2_delta", time_end = 100)

  cty$hospital_capacity <- cty$hospital_capacity * 2
  output2 <- daedalus(cty, "sars_cov_2_delta", time_end = 100)

  deaths <- get_epidemic_summary(output, "deaths")
  deaths2 <- get_epidemic_summary(output2, "deaths")

  expect_lt(
    deaths2$value,
    deaths$value
  )
})
