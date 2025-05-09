# Tests for the hospital capacity mechanism
# check that models run when hospital capacity is specified
test_that("Hospital capacity: basic expectations", {
  # NOTE: Not testing every country and infection
  response_strategy <- c("elimination", "school_closures", "economic_closures")
  cx <- daedalus_country("China")
  cx$hospital_capacity <- 100L
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

# NOTE: unable to find good tests for mortality rate changes

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
    response_time = 298
  )
  y <- daedalus(
    cty_y,
    "sars_cov_1",
    response_strategy = "elimination",
    response_time = 298
  )

  expect_lt(
    x$response_data$closure_info$closure_time_start,
    y$response_data$closure_info$closure_time_start
  )

  # hospital capacity override from `daedalus()`
  cty_x$hospital_capacity <- cty_y$hospital_capacity * 2 # 4x higher
  x <- daedalus(
    cty_x,
    "sars_cov_1",
    response_strategy = "elimination",
    response_time = 298
  )

  expect_gt(
    x$response_data$closure_info$closure_time_start,
    y$response_data$closure_info$closure_time_start
  )
})
