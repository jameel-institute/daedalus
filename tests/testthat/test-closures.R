# expectations that interventions work
response_names <- c(
  "none", "elimination",
  "economic_closures", "school_closures"
)
response_level <- c("light", "heavy")
country_canada <- country("Canada")

test_that("Closures: basic expectations: runs without errors", {
  # test all responses
  expect_no_condition({
    output_list <- lapply(response_names, function(x) {
      daedalus(country_canada, "sars_cov_1", response_strategy = x)
    })
  })

  # test some combinations of responses and implementation levels
  expect_no_condition({
    output_list <- Map(
      response_names, response_level,
      f = function(x, y) {
        daedalus(
          country_canada, "sars_cov_1",
          response_strategy = x, implementation_level = y
        )
      }
    )
  })
})

# expectation that response strategy 'none' has little to no effect
test_that("Closures: no response leads to similar epidemic sizes", {
  # both implementation levels have identical epidemic sizes
  levels <- c("light", "heavy")
  output_list <- lapply(levels, function(x) {
    daedalus(
      country_canada, "sars_cov_1",
      response_strategy = "none", implementation_level = x
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      sum(x[x$compartment == "recovered" & x$time == max(x$time), ]$value)
    }, numeric(1)
  )

  expect_identical(
    epidemic_sizes[1], epidemic_sizes[2]
  )

  # response time has no effect when strategy is 'none'
  response_times <- c(10, 100)
  response_threshold <- 1e7 # artificially large
  output_list <- lapply(response_times, function(x) {
    daedalus(
      country_canada, "sars_cov_1",
      response_strategy = "none", response_time = x,
      response_threshold = response_threshold
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      sum(x[x$compartment == "recovered" & x$time == max(x$time), ]$value)
    }, numeric(1)
  )
  # NOTE high tolerance as the two have just slightly different final sizes
  expect_identical(
    epidemic_sizes[1], epidemic_sizes[2],
    tolerance = 1L
  )

  # response threshold has no effect when strategy is 'none'
  response_thresholds <- c(10, 1000)
  time_end <- 300
  response_time <- time_end - 2L # artificially long response time
  output_list <- lapply(response_thresholds, function(x) {
    daedalus(
      country_canada, "sars_cov_1",
      response_strategy = "none", response_time = response_time,
      response_threshold = x
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      sum(x[x$compartment == "recovered" & x$time == max(x$time), ]$value)
    }, numeric(1)
  )

  # NOTE high tolerance as the two have just slightly different final sizes
  expect_identical(
    epidemic_sizes[1], epidemic_sizes[2],
    tolerance = 1L
  )
})

# expect that applying closures reduces epidemic size
test_that("Closures: basic statistical correctness: reduces epidemic size", {
  output_list <- lapply(response_names, function(x) {
    daedalus(
      country_canada, "sars_cov_1",
      response_strategy = x,
      implementation_level = "light", # test on light as this differs b/w strats
      infect_params_manual = list(rho = 0.0)
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      sum(x[x$compartment == "recovered" & x$time == max(x$time), ]$value)
    }, numeric(1)
  )

  expect_true(
    all(epidemic_sizes[-1] < epidemic_sizes[1])
  )
})

# expect that earlier closures reduce epidemic size
test_that("Closures: earlier closures reduce epidemic size", {
  response_times <- c(200, 30)
  response_threshold <- 1e9 # very high to prevent auto-activation
  output_list <- lapply(response_times, function(x) {
    daedalus(
      country_canada, "sars_cov_1",
      response_strategy = "elimination",
      response_time = x,
      response_threshold = response_threshold,
      implementation_level = "light",
      infect_params_manual = list(rho = 0.0)
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      sum(x[x$compartment == "recovered" & x$time == max(x$time), ]$value)
    }, numeric(1)
  )

  expect_true(
    epidemic_sizes[2] < epidemic_sizes[1]
  )
})

# expect that lower activation threshold reduces epidemic size
test_that("Closures: lower threshold reduces epidemic size", {
  response_thresholds <- c(1000, 100)
  output_list <- lapply(response_thresholds, function(x) {
    daedalus(
      country_canada, "sars_cov_1",
      response_strategy = "elimination",
      response_threshold = x,
      response_time = 200, # artificially high
      infect_params_manual = list(rho = 0.0)
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      sum(x[x$compartment == "recovered" & x$time == max(x$time), ]$value)
    }, numeric(1)
  )

  expect_true(
    epidemic_sizes[2] < epidemic_sizes[1]
  )
})
