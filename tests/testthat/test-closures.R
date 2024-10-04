# expectations that interventions work
response_names <- c(
  "none", "elimination",
  "economic_closures", "school_closures"
)
response_level <- c("light", "heavy")
country_x <- daedalus_country("Thailand")

test_that("Closures: basic expectations: runs without errors", {
  # test all responses
  expect_no_condition({
    output_list <- lapply(response_names, function(x) {
      daedalus(country_x, "sars_cov_1", response_strategy = x)
    })
  })

  # test some combinations of responses and implementation levels
  expect_no_condition({
    output_list <- Map(
      response_names, response_level,
      f = function(x, y) {
        daedalus(
          country_x, "sars_cov_1",
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
      country_x, "sars_cov_1",
      response_strategy = "none", implementation_level = x
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      x <- get_data(x)
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
      country_x, "sars_cov_1",
      response_strategy = "none", response_time = x,
      response_threshold = response_threshold
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      x <- get_data(x)
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
      country_x, "sars_cov_1",
      response_strategy = "none", response_time = response_time,
      response_threshold = x
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      x <- get_data(x)
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
      country_x, daedalus_infection("sars_cov_1", rho = 0.0),
      response_strategy = x,
      implementation_level = "light" # test on light as this differs b/w strats
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      x <- get_data(x)
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
      country_x, daedalus_infection("sars_cov_1", rho = 0.0),
      response_strategy = "elimination",
      response_time = x,
      response_threshold = response_threshold,
      implementation_level = "light"
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      x <- get_data(x)
      sum(x[x$compartment == "recovered" & x$time == max(x$time), ]$value)
    }, numeric(1)
  )

  expect_lt(
    epidemic_sizes[2], epidemic_sizes[1]
  )
})

# expect that lower activation threshold reduces epidemic size
test_that("Closures: lower threshold reduces epidemic size", {
  # NOTE: the relationship between repsonse threshold and the epidemic size
  # seems non-linear --- will open an issue for this
  response_thresholds <- c(10000, 100)
  output_list <- lapply(response_thresholds, function(x) {
    daedalus(
      country_x, daedalus_infection("sars_cov_1", rho = 0.0),
      response_strategy = "elimination",
      implementation_level = "heavy",
      response_threshold = x,
      response_time = 200 # artificially high
    )
  })

  epidemic_sizes <- vapply(
    output_list, function(x) {
      x <- get_data(x)
      sum(x[x$compartment == "recovered" & x$time == max(x$time), ]$value)
    }, numeric(1)
  )

  expect_lt(
    epidemic_sizes[2], epidemic_sizes[1]
  )
})

# check that closures are logged correctly
# NOTE: we can only really test closure start times, as closures either
# end reactively or do not end at all
test_that("Closures: correct logging of time limits", {
  response_threshold <- 1e9 # artificially large
  response_time <- 10 # arbitrary value
  time_end <- 25 # low to prematurely end simulation
  dummy_vax <- daedalus_vaccination("none", vax_start_time = 15)
  output <- daedalus(
    "Thailand", daedalus_infection("influenza_1918", r0 = 1.1),
    response_strategy = "elimination",
    response_threshold = response_threshold,
    response_time = response_time,
    vaccine_investment = dummy_vax,
    time_end = time_end
  )

  response_data <- get_data(output, "response_data")

  expect_identical(
    response_data[["closure_info"]][["closure_time_start"]],
    response_time
  )
  expect_identical(
    response_data[["closure_info"]][["closure_time_end"]],
    time_end
  )
})
